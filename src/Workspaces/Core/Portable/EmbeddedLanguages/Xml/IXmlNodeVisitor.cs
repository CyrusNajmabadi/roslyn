// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.CodeAnalysis.EmbeddedLanguages.Xml
{
    internal interface IXmlNodeVisitor
    {
        void Visit(XmlCompilationUnit node);
        //void Visit(XmlSequenceNode node);
//        void Visit(XmlElementNode node);
        void Visit(XmlGenericNode node);

        //void Visit(XmlElementStartTagNode node);
        //void Visit(XmlElementEndTagNode node);
        //void Visit(XmlEmptyElementNode node);
        //void Visit(XmlNameNode node);
        //void Visit(XmlPrefixNode node);
        //void Visit(XmlTextAttributeNode node);
        //void Visit(XmlCrefAttributeNode node);
        //void Visit(XmlNameAttributeNode node);
        //void Visit(XmlTextNode node);
        //void Visit(XmlCDataSectionNode node);
        //void Visit(XmlProcessingInstructionNode node);
        //void Visit(XmlCommentNode node);
    }
}
