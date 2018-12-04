// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Immutable;
using System.Composition;
using Microsoft.CodeAnalysis.Options;
using Microsoft.CodeAnalysis.Options.Providers;

namespace Microsoft.CodeAnalysis.EmbeddedLanguages.Xml.LanguageServices
{
    internal class XmlFeatureOptions
    {
        public static PerLanguageOption<bool> ColorizeXmlPatterns =
            new PerLanguageOption<bool>(
                nameof(XmlFeatureOptions),
                nameof(ColorizeXmlPatterns),
                defaultValue: true,
                storageLocations: new RoamingProfileStorageLocation("TextEditor.%LANGUAGE%.Specific.ColorizeXmlPatterns"));

        public static PerLanguageOption<bool> ReportInvalidXmlPatterns =
            new PerLanguageOption<bool>(
                nameof(XmlFeatureOptions),
                nameof(ReportInvalidXmlPatterns),
                defaultValue: true,
                storageLocations: new RoamingProfileStorageLocation("TextEditor.%LANGUAGE%.Specific.ReportInvalidXmlPatterns"));

        public static PerLanguageOption<bool> HighlightRelatedXmlComponentsUnderCursor =
            new PerLanguageOption<bool>(
                nameof(XmlFeatureOptions),
                nameof(HighlightRelatedXmlComponentsUnderCursor),
                defaultValue: true,
                storageLocations: new RoamingProfileStorageLocation("TextEditor.%LANGUAGE%.Specific.HighlightRelatedXmlComponentsUnderCursor"));

        public static PerLanguageOption<bool> DetectAndOfferEditorFeaturesForProbableXmlStrings =
            new PerLanguageOption<bool>(
                nameof(XmlFeatureOptions),
                nameof(DetectAndOfferEditorFeaturesForProbableXmlStrings),
                defaultValue: true,
                storageLocations: new RoamingProfileStorageLocation("TextEditor.%LANGUAGE%.Specific.DetectAndOfferEditorFeaturesForProbableXmlStrings"));
    }

    [ExportOptionProvider, Shared]
    internal class XmlFeatureOptionsProvider : IOptionProvider
    {
        public ImmutableArray<IOption> Options { get; } = ImmutableArray.Create<IOption>(
            XmlFeatureOptions.ColorizeXmlPatterns,
            XmlFeatureOptions.ReportInvalidXmlPatterns,
            XmlFeatureOptions.HighlightRelatedXmlComponentsUnderCursor,
            XmlFeatureOptions.DetectAndOfferEditorFeaturesForProbableXmlStrings);
    }
}
