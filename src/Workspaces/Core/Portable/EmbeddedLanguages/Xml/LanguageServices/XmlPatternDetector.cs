// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Text.RegularExpressions;
using System.Threading;
using System.Xml.Linq;
using Microsoft.CodeAnalysis.EmbeddedLanguages.LanguageServices;
using Microsoft.CodeAnalysis.LanguageServices;
using Microsoft.CodeAnalysis.Shared.Extensions;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.EmbeddedLanguages.Xml.LanguageServices
{
    /// <summary>
    /// Helper class to detect xml pattern tokens in a document efficiently.
    /// </summary>
    internal sealed class XmlPatternDetector
    {
        private const string _patternName = "pattern";

        private static readonly ConditionalWeakTable<SemanticModel, XmlPatternDetector> _modelToDetector =
            new ConditionalWeakTable<SemanticModel, XmlPatternDetector>();

        private readonly EmbeddedLanguageInfo _info;
        private readonly SemanticModel _semanticModel;
        private readonly ImmutableArray<INamedTypeSymbol> _xmlTypes;
        private readonly HashSet<string> _methodNamesOfInterest;
        private readonly IXmlParser _xmlParser;

        /// <summary>
        /// Helps match patterns of the form: language=regex,option1,option2,option3
        /// 
        /// All matching is case insensitive, with spaces allowed between the punctuation.
        /// 'regex' or 'regexp' are both allowed.  Option values will be or'ed together
        /// to produce final options value.  If an unknown option is encountered, processing
        /// will stop with whatever value has accumulated so far.
        /// 
        /// Option names are the values from the <see cref="LoadOptions"/> enum.
        /// </summary>
        private static readonly Regex s_languageCommentDetector =
            new Regex(@"\blang(uage)?\s*=\s*xml?\b((\s*,\s*)(?<option>[a-zA-Z]+))*",
                RegexOptions.ExplicitCapture | RegexOptions.IgnoreCase | RegexOptions.Compiled);

        private static readonly Dictionary<string, LoadOptions> s_nameToOption =
            typeof(LoadOptions).GetTypeInfo().DeclaredFields
                .Where(f => f.FieldType == typeof(LoadOptions))
                .ToDictionary(f => f.Name, f => (LoadOptions)f.GetValue(null), StringComparer.OrdinalIgnoreCase);

        public XmlPatternDetector(
            SemanticModel semanticModel,
            EmbeddedLanguageInfo info,
            ImmutableArray<INamedTypeSymbol> xmlTypes,
            HashSet<string> methodNamesOfInterest,
            IXmlParser xmlParser)
        {
            _info = info;
            _semanticModel = semanticModel;
            _xmlTypes = xmlTypes;
            _methodNamesOfInterest = methodNamesOfInterest;
            _xmlParser = xmlParser;
        }

        public static XmlPatternDetector TryGetOrCreate(
            Workspace workspace, SemanticModel semanticModel, EmbeddedLanguageInfo info)
        {
            // Do a quick non-allocating check first.
            if (_modelToDetector.TryGetValue(semanticModel, out var detector))
            {
                return detector;
            }

            return _modelToDetector.GetValue(
                semanticModel, _ => TryCreate(workspace, semanticModel, info));
        }

        private static XmlPatternDetector TryCreate(
            Workspace workspace, SemanticModel semanticModel, EmbeddedLanguageInfo info)
        {
            var xelementType = semanticModel.Compilation.GetTypeByMetadataName(typeof(XElement).FullName);
            var xdocumentType = semanticModel.Compilation.GetTypeByMetadataName(typeof(XDocument).FullName);
            if (xelementType == null || xdocumentType == null)
            {
                return null;
            }

            var languageServices = workspace.Services.GetLanguageServices(semanticModel.Language);
            if (languageServices == null)
            {
                return null;
            }

            var xmlParser = languageServices.GetService<IXmlParser>();
            if (xmlParser == null)
            {
                return null;
            }

            var types = ImmutableArray.Create(xelementType, xdocumentType);
            var methodNamesOfInterest = GetMethodNamesOfInterest(xelementType, info.SyntaxFacts);
            return new XmlPatternDetector(
                semanticModel, info, types, methodNamesOfInterest, xmlParser);
        }

        public static bool IsDefinitelyNotPattern(SyntaxToken token, ISyntaxFactsService syntaxFacts)
        {
            if (!syntaxFacts.IsStringLiteral(token))
            {
                return true;
            }

            if (!IsMethodOrConstructorArgument(token, syntaxFacts) &&
                !HasXmlLanguageComment(token, syntaxFacts, out _))
            {
                return true;
            }

            return false;
        }

        private static bool HasXmlLanguageComment(
            SyntaxToken token, ISyntaxFactsService syntaxFacts, out LoadOptions options)
        {
            if (HasXmlLanguageComment(token.GetPreviousToken().TrailingTrivia, syntaxFacts, out options))
            {
                return true;
            }

            for (var node = token.Parent; node != null; node = node.Parent)
            {
                if (HasXmlLanguageComment(node.GetLeadingTrivia(), syntaxFacts, out options))
                {
                    return true;
                }
            }

            options = default;
            return false;
        }

        private static bool HasXmlLanguageComment(
            SyntaxTriviaList list, ISyntaxFactsService syntaxFacts, out LoadOptions options)
        {
            foreach (var trivia in list)
            {
                if (HasXmlLanguageComment(trivia, syntaxFacts, out options))
                {
                    return true;
                }
            }

            options = default;
            return false;
        }

        private static bool HasXmlLanguageComment(
            SyntaxTrivia trivia, ISyntaxFactsService syntaxFacts, out LoadOptions options)
        {
            if (syntaxFacts.IsRegularComment(trivia))
            {
                // Note: ToString on SyntaxTrivia is non-allocating.  It will just return the
                // underlying text that the trivia is already pointing to.
                var text = trivia.ToString();
                var (matched, matchOptions) = TryMatch(text);
                if (matched)
                {
                    options = matchOptions;
                    return true;
                }
            }

            options = default;
            return false;
        }

        private static (bool success, LoadOptions options) TryMatch(string text)
        {
            var options = LoadOptions.None;
            var match = s_languageCommentDetector.Match(text);
            if (!match.Success)
            {
                return default;
            }

            var optionGroup = match.Groups["option"];
            foreach (Capture capture in optionGroup.Captures)
            {
                if (s_nameToOption.TryGetValue(capture.Value, out var specificOption))
                {
                    options |= specificOption;
                }
                else
                {
                    // hit something we don't understand.  bail out.  that will help ensure
                    // users don't have weird behavior just because they misspelled something.
                    // instead, they will know they need to fix it up.
                    return default;
                }
            }

            return (true, options);
        }

        private static bool IsMethodOrConstructorArgument(SyntaxToken token, ISyntaxFactsService syntaxFacts)
            => syntaxFacts.IsLiteralExpression(token.Parent) &&
               syntaxFacts.IsArgument(token.Parent.Parent);

        private static HashSet<string> GetMethodNamesOfInterest(INamedTypeSymbol regexType, ISyntaxFactsService syntaxFacts)
        {
            return new HashSet<string> { nameof(XElement.Parse) };
        }

        public bool IsXmlPattern(SyntaxToken token, CancellationToken cancellationToken, out LoadOptions options)
        {
            options = default;
            if (IsDefinitelyNotPattern(token, _info.SyntaxFacts))
            {
                return false;
            }

            var syntaxFacts = _info.SyntaxFacts;
            if (HasXmlLanguageComment(token, syntaxFacts, out options))
            {
                return true;
            }

            var stringLiteral = token;
            var literalNode = stringLiteral.Parent;
            var argumentNode = literalNode.Parent;
            Debug.Assert(syntaxFacts.IsArgument(argumentNode));

            var argumentList = argumentNode.Parent;
            var invocationOrCreation = argumentList.Parent;
            if (syntaxFacts.IsInvocationExpression(invocationOrCreation))
            {
                var invokedExpression = syntaxFacts.GetExpressionOfInvocationExpression(invocationOrCreation);
                var name = GetNameOfInvokedExpression(invokedExpression);
                if (_methodNamesOfInterest.Contains(name))
                {
                    // Is a string argument to a method that looks like it could be a xml method.  
                    // Need to do deeper analysis.

                    // Note we do not use GetAllSymbols here because we don't want to incur the
                    // allocation.
                    var symbolInfo = _semanticModel.GetSymbolInfo(invocationOrCreation, cancellationToken);
                    var method = symbolInfo.Symbol;
                    if (TryAnalyzeInvocation(stringLiteral, argumentNode, method, cancellationToken, out options))
                    {
                        return true;
                    }

                    foreach (var candidate in symbolInfo.CandidateSymbols)
                    {
                        if (TryAnalyzeInvocation(stringLiteral, argumentNode, candidate, cancellationToken, out options))
                        {
                            return true;
                        }
                    }
                }
            }

            return false;
        }

        private bool TryAnalyzeInvocation(
            SyntaxToken stringLiteral, SyntaxNode argumentNode, ISymbol method,
            CancellationToken cancellationToken, out LoadOptions options)
        {
            if (method != null &&
                method.DeclaredAccessibility == Accessibility.Public &&
                method.IsStatic &&
                _xmlTypes.Contains(method.ContainingType))
            {
                return AnalyzeStringLiteral(
                    stringLiteral, argumentNode, cancellationToken, out options);
            }

            options = default;
            return false;
        }

        public XmlTree TryParseXmlPattern(SyntaxToken token, CancellationToken cancellationToken)
        {
            if (!this.IsXmlPattern(token, cancellationToken, out var options))
            {
                return null;
            }

            var chars = _info.VirtualCharService.TryConvertToVirtualChars(token);
            if (chars.IsDefaultOrEmpty)
            {
                return null;
            }

            return _xmlParser.TryParse(chars, options);
        }

        private bool AnalyzeStringLiteral(
            SyntaxToken stringLiteral, SyntaxNode argumentNode,
            CancellationToken cancellationToken, out LoadOptions options)
        {
            options = default;

            var parameter = _info.SemanticFacts.FindParameterForArgument(_semanticModel, argumentNode, cancellationToken);
            if (parameter?.Name != _patternName)
            {
                return false;
            }

            options = GetLoadOptions(argumentNode, cancellationToken);
            return true;
        }

        private LoadOptions GetLoadOptions(SyntaxNode argumentNode, CancellationToken cancellationToken)
        {
            var syntaxFacts = _info.SyntaxFacts;
            var argumentList = argumentNode.Parent;
            var arguments = syntaxFacts.GetArgumentsOfArgumentList(argumentList);
            foreach (var siblingArg in arguments)
            {
                if (siblingArg != argumentNode)
                {
                    var expr = syntaxFacts.GetExpressionOfArgument(siblingArg);
                    if (expr != null)
                    {
                        var exprType = _semanticModel.GetTypeInfo(expr, cancellationToken);
                        if (exprType.Type?.Name == nameof(LoadOptions))
                        {
                            var constVal = _semanticModel.GetConstantValue(expr, cancellationToken);
                            if (constVal.HasValue)
                            {
                                return (LoadOptions)(int)constVal.Value;
                            }
                        }
                    }
                }
            }

            return LoadOptions.None;
        }

        private string GetNameOfType(SyntaxNode typeNode, ISyntaxFactsService syntaxFacts)
        {
            if (syntaxFacts.IsQualifiedName(typeNode))
            {
                return GetNameOfType(syntaxFacts.GetRightSideOfDot(typeNode), syntaxFacts);
            }
            else if (syntaxFacts.IsIdentifierName(typeNode))
            {
                return syntaxFacts.GetIdentifierOfSimpleName(typeNode).ValueText;
            }

            return null;
        }

        private string GetNameOfInvokedExpression(SyntaxNode invokedExpression)
        {
            var syntaxFacts = _info.SyntaxFacts;
            if (syntaxFacts.IsSimpleMemberAccessExpression(invokedExpression))
            {
                return syntaxFacts.GetIdentifierOfSimpleName(syntaxFacts.GetNameOfMemberAccessExpression(invokedExpression)).ValueText;
            }
            else if (syntaxFacts.IsIdentifierName(invokedExpression))
            {
                return syntaxFacts.GetIdentifierOfSimpleName(invokedExpression).ValueText;
            }

            return null;
        }

        internal static class TestAccessor
        {
            public static (bool success, LoadOptions options) TryMatch(string text)
                => XmlPatternDetector.TryMatch(text);
        }
    }
}
