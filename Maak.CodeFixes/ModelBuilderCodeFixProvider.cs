using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Maak.CodeFixes.ModelBuilder;
using Maak.ModelBuilder;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Simplification;
using Microsoft.CodeAnalysis.Text;
using SyntaxFactory = Microsoft.CodeAnalysis.CSharp.SyntaxFactory;
using SyntaxKind = Microsoft.CodeAnalysis.CSharp.SyntaxKind;

namespace Maak.CodeFixes
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(ModelBuilderCodeFixProvider)), Shared]
    public class ModelBuilderCodeFixProvider : CodeFixProvider
    {
        public sealed override ImmutableArray<string> FixableDiagnosticIds
            => ImmutableArray.Create(ModelBuilderAnalyzer.DiagnosticId);

        public sealed override FixAllProvider GetFixAllProvider()
        {
            // See https://github.com/dotnet/roslyn/blob/main/docs/analyzers/FixAllProvider.md for more information on Fix All Providers
            return WellKnownFixAllProviders.BatchFixer;
        }

        public sealed override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);

            // TODO: Replace the following code with your own analysis, generating a CodeAction for each fix to suggest
            var diagnostic = context.Diagnostics.First();
            var diagnosticSpan = diagnostic.Location.SourceSpan;

            // Find the type declaration identified by the diagnostic.
            var declaration = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf().OfType<TypeDeclarationSyntax>().First();

            // Register a code action that will invoke the fix.
            context.RegisterCodeFix(
                CodeAction.Create(
                    title: CodeFixResources.ModelBuilderTitle,
                    createChangedDocument: async (c) => await GenerateMappingMethodBody(context.Document, declaration, c).ConfigureAwait(false),
                    equivalenceKey: nameof(CodeFixResources.ModelBuilderTitle)),
                diagnostic);
        }

        private async Task<Document> GenerateMappingMethodBody(Document document, TypeDeclarationSyntax typeDecl, CancellationToken cancellationToken)
        {
            var semanticModel = await document.GetSemanticModelAsync(cancellationToken);
            var modelBuilderDef = semanticModel.GetDeclaredSymbol(typeDecl, cancellationToken);
            var genericBogusBuilderBaseType = (modelBuilderDef as INamedTypeSymbol)?.BaseType;

            var model = genericBogusBuilderBaseType.TypeArguments.First();

            var codeModelBuilderGenerator = new ModelBuilderGenerator();
            var typedef = codeModelBuilderGenerator.MapTypeDeclarationSyntax(model as INamedTypeSymbol);
            var source = codeModelBuilderGenerator.GetSourceFile(typedef, new List<TypeDefinition>());
            var sourceCode = SourceText.From(SyntaxFactory.ParseCompilationUnit(source).NormalizeWhitespace().ToString());
            return document.WithText(sourceCode);
        }
    }
}