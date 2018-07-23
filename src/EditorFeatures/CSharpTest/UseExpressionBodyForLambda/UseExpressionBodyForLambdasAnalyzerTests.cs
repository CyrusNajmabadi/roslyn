// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Generic;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.CodeStyle;
using Microsoft.CodeAnalysis.CSharp.UseExpressionBodyForLambda;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Editor.CSharp.UnitTests.Diagnostics;
using Microsoft.CodeAnalysis.Options;
using Microsoft.CodeAnalysis.Test.Utilities;
using Xunit;

namespace Microsoft.CodeAnalysis.Editor.CSharp.UnitTests.UseExpressionBody
{
    public class UseExpressionBodyForLambdasAnalyzerTests : AbstractCSharpDiagnosticProviderBasedUserDiagnosticTest
    {
        internal override (DiagnosticAnalyzer, CodeFixProvider) CreateDiagnosticProviderAndFixer(Workspace workspace)
            => (new UseExpressionBodyForLambdaDiagnosticAnalyzer(), new UseExpressionBodyForLambdaCodeFixProvider());

        private IDictionary<OptionKey, object> UseExpressionBody =>
            this.Option(CSharpCodeStyleOptions.PreferExpressionBodiedLambdaExpressions, CSharpCodeStyleOptions.WhenPossibleWithSuggestionEnforcement);

        private IDictionary<OptionKey, object> UseBlockBody =>
            this.Option(CSharpCodeStyleOptions.PreferExpressionBodiedLambdaExpressions, CSharpCodeStyleOptions.NeverWithSuggestionEnforcement);

        private async Task TestBoundAndUnboundAsync(string initialMarkup, string expectedMarkup, IDictionary<OptionKey, object> options)
        {
            await TestInRegularAndScriptAsync(
                WrapStatementContext(initialMarkup, true),
                WrapStatementContext(expectedMarkup, true),
                options: options);

            await TestInRegularAndScriptAsync(
                WrapStatementContext(initialMarkup, false),
                WrapStatementContext(expectedMarkup, false),
                options: options);

            string WrapStatementContext(string statementContextMarkup, bool withUsingSystem)
            {
                return (withUsingSystem ? "using System;\r\n" : null) +
@"using System.Threading.Tasks;

class C
{
    void Goo()
    {
" + statementContextMarkup + @"
    }
}";
            }
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseExpressionBodyInFieldInitializer()
        {
            await TestBoundAndUnboundAsync(
@"Func<int, string> f = x [|=>|]
{
    return x.ToString();
};",
@"Func<int, string> f = x => x.ToString();", options: UseExpressionBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseBlockBodyInFieldInitializer()
        {
            await TestBoundAndUnboundAsync(
@"Func<int, string> f = x [|=>|] x.ToString();",
@"Func<int, string> f = x =>
{
    return x.ToString();
};", options: UseBlockBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseExpressionBodyInArgument()
        {
            await TestInRegularAndScriptAsync(
@"using System;

class C
{
    void Goo()
    {
        TargetMethod(x [|=>|]
        {
            return x.ToString();
        });
    }

    void TargetMethod(Func<int, string> targetParam) { }
}",
@"using System;

class C
{
    void Goo()
    {
        TargetMethod(x => x.ToString());
    }

    void TargetMethod(Func<int, string> targetParam) { }
}", options: UseExpressionBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseBlockBodyInArgument()
        {
            await TestInRegularAndScriptAsync(
@"using System;

class C
{
    void Goo()
    {
        TargetMethod(x [|=>|] x.ToString());
    }

    void TargetMethod(Func<int, string> targetParam) { }
}",
@"using System;

class C
{
    void Goo()
    {
        TargetMethod(x =>
        {
            return x.ToString();
        });
    }

    void TargetMethod(Func<int, string> targetParam) { }
}", options: UseBlockBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseExpressionBodyFromReturnKeyword()
        {
            await TestMissingInRegularAndScriptAsync(
@"using System;

class C
{
    void Goo()
    {
        Func<int, string> f = x =>
        {
            [|return|] x.ToString();
        };
    }
}", new TestParameters(options: UseExpressionBody));
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseExpressionBodyFromLambdaOpeningBrace()
        {
            await TestMissingInRegularAndScriptAsync(
@"using System;

class C
{
    void Goo()
    {
        Func<int, string> f = x =>
        [|{|]
            return x.ToString();
        };
    }
}", new TestParameters(options: UseExpressionBody));
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseExpressionBodyFromLambdaClosingBrace()
        {
            await TestMissingInRegularAndScriptAsync(
@"using System;

class C
{
    void Goo()
    {
        Func<int, string> f = x =>
        {
            return x.ToString();
        [|}|];
    }
}", new TestParameters(options: UseExpressionBody));
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseExpressionBodyThrowing()
        {
            await TestBoundAndUnboundAsync(
@"Func<int, string> f = x [|=>|]
{
    throw null;
};",
@"Func<int, string> f = x => throw null;", options: UseExpressionBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseBlockBodyThrowing()
        {
            await TestBoundAndUnboundAsync(
@"Func<int, string> f = x [|=>|] throw null;",
@"Func<int, string> f = x =>
{
    throw null;
};", options: UseBlockBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseExpressionBodyWithVoidReturn()
        {
            await TestBoundAndUnboundAsync(
@"Action<int> f = x [|=>|]
{
    x.ToString();
};",
@"Action<int> f = x => x.ToString();", options: UseExpressionBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseExpressionBodyWithVoidReturnThrowing()
        {
            await TestBoundAndUnboundAsync(
@"Action<int> f = x [|=>|]
{
    throw null;
};",
@"Action<int> f = x => throw null;", options: UseExpressionBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseBlockBodyWithVoidReturn()
        {
            await TestBoundAndUnboundAsync(
@"Action<int> f = x [|=>|] x.ToString();",
@"Action<int> f = x =>
{
    x.ToString();
};", options: UseBlockBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseBlockBodyWithVoidReturnThrowing()
        {
            await TestBoundAndUnboundAsync(
@"Action<int> f = x [|=>|] throw null;",
@"Action<int> f = x =>
{
    throw null;
};", options: UseBlockBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseExpressionBodyWithAsyncVoidReturn()
        {
            await TestBoundAndUnboundAsync(
@"Action<int> f = async x [|=>|]
{
    x.ToString();
};",
@"Action<int> f = async x => x.ToString();", options: UseExpressionBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseExpressionBodyWithAsyncVoidReturnThrowing()
        {
            await TestBoundAndUnboundAsync(
@"Action<int> f = async x [|=>|]
{
    throw null;
};",
@"Action<int> f = async x => throw null;", options: UseExpressionBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseBlockBodyWithAsyncVoidReturn()
        {
            await TestBoundAndUnboundAsync(
@"Action<int> f = async x [|=>|] x.ToString();",
@"Action<int> f = async x =>
{
    x.ToString();
};", options: UseBlockBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseBlockBodyWithAsyncVoidReturnThrowing()
        {
            await TestBoundAndUnboundAsync(
@"Action<int> f = async x [|=>|] throw null;",
@"Action<int> f = async x =>
{
    throw null;
};", options: UseBlockBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseExpressionBodyWithTaskReturn()
        {
            await TestBoundAndUnboundAsync(
@"Func<Task> f = () [|=>|]
{
    return Task.CompletedTask;
};",
@"Func<Task> f = () => Task.CompletedTask;", options: UseExpressionBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseExpressionBodyWithTaskReturnThrowing()
        {
            await TestBoundAndUnboundAsync(
@"Func<Task> f = () [|=>|]
{
    throw null;
};",
@"Func<Task> f = () => throw null;", options: UseExpressionBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseBlockBodyWithTaskReturn()
        {
            await TestBoundAndUnboundAsync(
@"Func<Task> f = () [|=>|] Task.CompletedTask;",
@"Func<Task> f = () =>
{
    return Task.CompletedTask;
};", options: UseBlockBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseBlockBodyWithTaskReturnThrowing()
        {
            await TestBoundAndUnboundAsync(
@"Func<Task> f = () [|=>|] throw null;",
@"Func<Task> f = () =>
{
    throw null;
};", options: UseBlockBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseExpressionBodyWithAsyncTaskReturn()
        {
            await TestBoundAndUnboundAsync(
@"Func<Task> f = async () [|=>|]
{
    await Task.CompletedTask;
};",
@"Func<Task> f = async () => await Task.CompletedTask;", options: UseExpressionBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseExpressionBodyWithAsyncTaskReturnThrowing()
        {
            await TestBoundAndUnboundAsync(
@"Func<Task> f = async () [|=>|]
{
    throw null;
};",
@"Func<Task> f = async () => throw null;", options: UseExpressionBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseBlockBodyWithAsyncTaskReturn()
        {
            await TestBoundAndUnboundAsync(
@"Func<Task> f = async () [|=>|] await Task.CompletedTask;",
@"Func<Task> f = async () =>
{
    await Task.CompletedTask;
};", options: UseBlockBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseBlockBodyWithAsyncTaskReturnThrowing()
        {
            await TestBoundAndUnboundAsync(
@"Func<Task> f = async () [|=>|] throw null;",
@"Func<Task> f = async () =>
{
    throw null;
};", options: UseBlockBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseExpressionBodyWithTaskTReturn()
        {
            await TestBoundAndUnboundAsync(
@"Func<int, Task<string>> f = x [|=>|]
{
    return Task.FromResult(x.ToString());
};",
@"Func<int, Task<string>> f = x => Task.FromResult(x.ToString());", options: UseExpressionBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseExpressionBodyWithTaskTReturnThrowing()
        {
            await TestBoundAndUnboundAsync(
@"Func<int, Task<string>> f = x [|=>|]
{
    throw null;
};",
@"Func<int, Task<string>> f = x => throw null;", options: UseExpressionBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseBlockBodyWithTaskTReturn()
        {
            await TestBoundAndUnboundAsync(
@"Func<int, Task<string>> f = x [|=>|] Task.FromResult(x.ToString());",
@"Func<int, Task<string>> f = x =>
{
    return Task.FromResult(x.ToString());
};", options: UseBlockBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseBlockBodyWithTaskTReturnThrowing()
        {
            await TestBoundAndUnboundAsync(
@"Func<int, Task<string>> f = x [|=>|] throw null;",
@"Func<int, Task<string>> f = x =>
{
    throw null;
};", options: UseBlockBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseExpressionBodyWithAsyncTaskTReturn()
        {
            await TestBoundAndUnboundAsync(
@"Func<int, Task<string>> f = async x [|=>|]
{
    return await Task.FromResult(x.ToString());
};",
@"Func<int, Task<string>> f = async x => await Task.FromResult(x.ToString());", options: UseExpressionBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseExpressionBodyWithAsyncTaskTReturnThrowing()
        {
            await TestBoundAndUnboundAsync(
@"Func<int, Task<string>> f = async x [|=>|]
{
    throw null;
};",
@"Func<int, Task<string>> f = async x => throw null;", options: UseExpressionBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseBlockBodyWithAsyncTaskTReturn()
        {
            await TestBoundAndUnboundAsync(
@"Func<int, Task<string>> f = async x [|=>|] await Task.FromResult(x.ToString());",
@"Func<int, Task<string>> f = async x =>
{
    return await Task.FromResult(x.ToString());
};", options: UseBlockBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseBlockBodyWithAsyncTaskTReturnThrowing()
        {
            await TestBoundAndUnboundAsync(
@"Func<int, Task<string>> f = async x [|=>|] throw null;",
@"Func<int, Task<string>> f = async x =>
{
    throw null;
};", options: UseBlockBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseExpressionBodyWithPrecedingComment()
        {
            await TestInRegularAndScriptAsync(
@"using System;
using System.Threading.Tasks;

class C
{
    void Goo()
    {
        Func<int, string> f = x [|=>|]
        {
            // Comment
            return x.ToString();
        };
    }
}",
@"using System;
using System.Threading.Tasks;

class C
{
    void Goo()
    {
        Func<int, string> f = x =>
            // Comment
            x.ToString();
    }
}", options: UseExpressionBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseExpressionBodyWithEndingComment()
        {
            await TestInRegularAndScriptAsync(
@"using System;
using System.Threading.Tasks;

class C
{
    void Goo()
    {
        Func<int, string> f = x [|=>|]
        {
            return x.ToString(); // Comment
        };
    }
}",
@"using System;
using System.Threading.Tasks;

class C
{
    void Goo()
    {
        Func<int, string> f = x => x.ToString();
    }
}", options: UseExpressionBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseBlockBodyWithEndingComment()
        {
            await TestInRegularAndScriptAsync(
@"using System;
using System.Threading.Tasks;

class C
{
    void Goo()
    {
        Func<int, string> f = x [|=>|] x.ToString(); // Comment
    }
}",
@"using System;
using System.Threading.Tasks;

class C
{
    void Goo()
    {
        Func<int, string> f = x =>
        {
            return x.ToString();
        }; // Comment
    }
}", options: UseBlockBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseExpressionBodyWithPreprocessor()
        {
            await TestMissingAsync(
@"using System;
using System.Threading.Tasks;

class C
{
    void Goo()
    {
        Func<int, string> f = x [|=>|]
        {
#if DEBUG
#endif
            return x.ToString();
        };
    }
}");
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseExpressionBodyWithPrecedingEmptyStatement()
        {
            await TestMissingAsync(
@"using System;
using System.Threading.Tasks;

class C
{
    void Goo()
    {
        Func<int, string> f = x [|=>|]
        {
            ;
            return x.ToString();
        };
    }
}");
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseExpressionBodyWithFololowingEmptyStatement()
        {
            await TestMissingAsync(
@"using System;
using System.Threading.Tasks;

class C
{
    void Goo()
    {
        Func<int, string> f = x [|=>|]
        {
            return x.ToString();
            ;
        };
    }
}");
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseExpressionBodyWithNonExpressionStatement()
        {
            await TestMissingAsync(
@"using System;
using System.Threading.Tasks;

class C
{
    void Goo()
    {
        Func<int, string> f = x [|=>|]
        {
            if (true) return x.ToString();
        };
    }
}");
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseExpressionBodyWithNoStatements()
        {
            await TestMissingAsync(
@"using System;
using System.Threading.Tasks;

class C
{
    void Goo()
    {
        Func<int, string> f = x [|=>|]
        {
            // Comment
        };
    }
}");
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseBlockBodyWithFollowingStatementOnSameLine()
        {
            await TestInRegularAndScriptAsync(
@"using System;
using System.Threading.Tasks;

class C
{
    void Goo()
    {
        Func<int, string> f = x [|=>|] x.ToString(); 1.ToString();
    }
}",
@"using System;
using System.Threading.Tasks;

class C
{
    void Goo()
    {
        Func<int, string> f = x =>
        {
            return x.ToString();
        }; 1.ToString();
    }
}", options: UseBlockBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task UseExpressionBodyWithBlockBodyOnOneLine()
        {
            await TestInRegularAndScriptAsync(
@"using System;

class C
{
    void Goo()
    {
        Func<int, string> f = x [|=>|] { return x.ToString(); };
    }
}",
@"using System;

class C
{
    void Goo()
    {
        Func<int, string> f = x => x.ToString();
    }
}", options: UseExpressionBody);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task OfferToConvertThrowExpressionToBlockPriorToCSharp6()
        {
            await TestAsync(
@"using System;
using System.Threading.Tasks;

class C
{
    void Goo()
    {
        Func<int, Task<string>> f = x [|=>|] throw null;
    }
}",
@"using System;
using System.Threading.Tasks;

class C
{
    void Goo()
    {
        Func<int, Task<string>> f = x =>
        {
            throw null;
        };
    }
}", options: UseBlockBody, parseOptions: CSharpParseOptions.Default.WithLanguageVersion(LanguageVersion.CSharp5));
        }

        [Fact, Trait(Traits.Feature, Traits.Features.CodeActionsUseExpressionBody)]
        public async Task DoNotOfferToConvertBlockWithThrowToExpressionIfCSharp6()
        {
            await TestMissingAsync(
@"using System;
using System.Threading.Tasks;

class C
{
    void Goo()
    {
        Func<int, Task<string>> f = x [|=>|]
        {
            throw null;
        };
    }
}", new TestParameters(options: UseExpressionBody, parseOptions: CSharpParseOptions.Default.WithLanguageVersion(LanguageVersion.CSharp6)));
        }
    }
}
