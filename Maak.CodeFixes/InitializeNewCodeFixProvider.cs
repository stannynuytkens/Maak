using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Maak.InitializeNew;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Simplification;
using SyntaxFactory = Microsoft.CodeAnalysis.CSharp.SyntaxFactory;
using SyntaxKind = Microsoft.CodeAnalysis.CSharp.SyntaxKind;

namespace Maak.CodeFixes
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(InitializeNewCodeFixProvider)), Shared]
    public class InitializeNewCodeFixProvider : CodeFixProvider
    {
        public sealed override ImmutableArray<string> FixableDiagnosticIds
            => ImmutableArray.Create(InitializeNewAnalyzer.DiagnosticId);

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
            var declaration = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf()
                .OfType<LocalDeclarationStatementSyntax>().First();

            // Register a code action that will invoke the fix.
            context.RegisterCodeFix(
                CodeAction.Create(
                    title: CodeFixResources.InitializeNewTitle,
                    createChangedDocument: c => InitializeNewAsync(context, declaration, c),
                    equivalenceKey: nameof(CodeFixResources.InitializeNewTitle)),
                diagnostic);
        }

        // http://roslynquoter.azurewebsites.net/
        private static async Task<Document> InitializeNewAsync(CodeFixContext context,
            LocalDeclarationStatementSyntax localDeclaration,
            CancellationToken cancellationToken)
        {
            var document = context.Document;
            var semanticModel = await document.GetSemanticModelAsync(cancellationToken);
            var typeDeclaration = localDeclaration.Declaration.Type;
            var symbolInfo = semanticModel?.GetSymbolInfo(typeDeclaration);
            var typeSymbol = symbolInfo?.Symbol;

            if (typeSymbol == null)
                return await Task.FromResult(document);

            var namedSymbol = semanticModel.Compilation.GetTypeByMetadataName(typeSymbol.MetadataName);

            var hasDefaultConstructor = (namedSymbol?.Constructors)?.SingleOrDefault(c => !c.Parameters.Any()) != null;
            // should use SymbolEqualityComparer(.Default) here?

#pragma warning disable RS1024 // Compare symbols correctly
            var properties = namedSymbol?.GetMembers()
                .Where(m => m.Kind == SymbolKind.Property
                            && m.DeclaredAccessibility == Accessibility.Public
                            && !((IPropertySymbol)m).IsReadOnly
                            && !((IPropertySymbol)m).IsStatic)
                .ToDictionary(k => k.Name, v => ((IPropertySymbol)v).Type);
#pragma warning restore RS1024 // Compare symbols correctly

            var hasValidProperties = properties?.Any() != false;

            if (!hasValidProperties)
                return await Task.FromResult(document);

            var initializer = (localDeclaration.Declaration.Variables.FirstOrDefault()?.Initializer?.Value
                                  as ObjectCreationExpressionSyntax)?.Initializer ??
                              SyntaxFactory.InitializerExpression(SyntaxKind.WithInitializerExpression);
            var initializerExpressions = initializer?.Expressions;

            var uninitializedPropertyNames = properties?
                .Select(p => p.Key)
                .Except(initializerExpressions.Value.ToList()
                    .Select(ip => (ip as AssignmentExpressionSyntax)?.Left.ToString()))
                .ToList();

            // all properties already exist in the initializer
            if (uninitializedPropertyNames?.Count == 0)
                return await Task.FromResult(document);

            var propertiesToInitialize = properties?.Where(p => uninitializedPropertyNames.Contains(p.Key));

            foreach (var propertyToInitialize in
                propertiesToInitialize ?? new List<KeyValuePair<string, ITypeSymbol>>())
            {
                // var value = SyntaxFactory.LiteralExpression(SyntaxKind.NullLiteralExpression);
                // if (propertyToInitialize.Value.IsValueType &&
                //     propertyToInitialize.Value.NullableAnnotation != NullableAnnotation.Annotated)
                // {
                //     // non-nullable value type
                //     var typedValue = TypeToDefaultValue(propertyToInitialize.Value);
                // }

                var newAssignment = SyntaxFactory.AssignmentExpression(
                    SyntaxKind.SimpleAssignmentExpression,
                    SyntaxFactory.IdentifierName(propertyToInitialize.Key),
                    TypeToDefaultValue(propertyToInitialize.Value));

                // TODO: immutable!
                initializerExpressions.Value.Add(newAssignment);
            }

            var firstToken = localDeclaration.GetFirstToken();
            var leadingTrivia = firstToken.LeadingTrivia;
            var trimmedLocal = localDeclaration.ReplaceToken(
                firstToken, firstToken.WithLeadingTrivia(SyntaxTriviaList.Empty));

            var declaration = localDeclaration.Declaration;
            var declarator = declaration.Variables.First();
            // var newLocal = trimmedLocal.WithDeclaration(declarator.WithInitializer(initializer.WithExpressions(initializerExpressions)));
            //
            // // Add an annotation to format the new local declaration.
            // LocalDeclarationStatementSyntax formattedLocal = newLocal.WithAdditionalAnnotations(Formatter.Annotation);

            var oldRoot = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);           

            // (localDeclaration.Declaration.Variables.FirstOrDefault()?.Initializer?.Value
            //     as ObjectCreationExpressionSyntax)?.Initializer?.Expressions.Replace();

            // var newLocal = trimmedLocal.WithModifiers(newModifiers).WithDeclaration(variableDeclaration);
            // var formattedLocal = newLocal.WithAdditionalAnnotations(Formatter.Annotation);

            // var oldRoot = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
            // var newRoot = oldRoot.ReplaceNode(localDeclaration, formattedLocal);


            // don't do anything yet..
            return await Task.FromResult(document);

            // // Remove the leading trivia from the local declaration.
            // SyntaxToken firstToken = localDeclaration.GetFirstToken();
            // SyntaxTriviaList leadingTrivia = firstToken.LeadingTrivia;
            // LocalDeclarationStatementSyntax trimmedLocal = localDeclaration.ReplaceToken(
            //     firstToken, firstToken.WithLeadingTrivia(SyntaxTriviaList.Empty));
            //
            // // Create a const token with the leading trivia.
            // SyntaxToken constToken = SyntaxFactory.Token(leadingTrivia, SyntaxKind.ConstKeyword, SyntaxFactory.TriviaList(SyntaxFactory.ElasticMarker));
            //
            // // Insert the const token into the modifiers list, creating a new modifiers list.
            // SyntaxTokenList newModifiers = trimmedLocal.Modifiers.Insert(0, constToken);
            //
            // // If the type of the declaration is 'var', create a new type name
            // // for the inferred type.
            // VariableDeclarationSyntax variableDeclaration = localDeclaration.Declaration;
            // TypeSyntax variableTypeName = variableDeclaration.Type;
            // if (variableTypeName.IsVar)
            // {
            //     SemanticModel semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);
            //
            //     // Special case: Ensure that 'var' isn't actually an alias to another type
            //     // (e.g. using var = System.String).
            //     IAliasSymbol aliasInfo = semanticModel.GetAliasInfo(variableTypeName, cancellationToken);
            //     if (aliasInfo == null)
            //     {
            //         // Retrieve the type inferred for var.
            //         ITypeSymbol type = semanticModel.GetTypeInfo(variableTypeName, cancellationToken).ConvertedType;
            //
            //         // Special case: Ensure that 'var' isn't actually a type named 'var'.
            //         if (type.Name != "var")
            //         {
            //             // Create a new TypeSyntax for the inferred type. Be careful
            //             // to keep any leading and trailing trivia from the var keyword.
            //             TypeSyntax typeName = SyntaxFactory.ParseTypeName(type.ToDisplayString())
            //                 .WithLeadingTrivia(variableTypeName.GetLeadingTrivia())
            //                 .WithTrailingTrivia(variableTypeName.GetTrailingTrivia());
            //
            //             // Add an annotation to simplify the type name.
            //             TypeSyntax simplifiedTypeName = typeName.WithAdditionalAnnotations(Simplifier.Annotation);
            //
            //             // Replace the type in the variable declaration.
            //             variableDeclaration = variableDeclaration.WithType(simplifiedTypeName);
            //         }
            //     }
            // }
            // // Produce the new local declaration.
            // LocalDeclarationStatementSyntax newLocal = trimmedLocal.WithModifiers(newModifiers)
            //                            .WithDeclaration(variableDeclaration);
            //
            // // Add an annotation to format the new local declaration.
            // LocalDeclarationStatementSyntax formattedLocal = newLocal.WithAdditionalAnnotations(Formatter.Annotation);
            //
            // // Replace the old local declaration with the new local declaration.
            // SyntaxNode oldRoot = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
            // SyntaxNode newRoot = oldRoot.ReplaceNode(localDeclaration, formattedLocal);
            //
            // // Return document with transformed tree.
            // return document.WithSyntaxRoot(newRoot);
        }

        private static ExpressionSyntax TypeToDefaultValue(ITypeSymbol typeSymbol, bool isExplicit = false)
        {
            var syntax = TypeToDefaultValueAux(typeSymbol, isExplicit, out bool cannotBeExplicit);
            if (isExplicit && cannotBeExplicit)
            {
                syntax = TypeToDefaultValueAux(typeSymbol, false, out _);
            }

            return syntax;
        }

        private static ExpressionSyntax TypeToDefaultValueAux(ITypeSymbol typeSymbol, bool isExplicit,
            out bool cannotBeExplicit)
        {
            // https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/default-values-table
            // https://docs.microsoft.com/tr-tr/dotnet/csharp/language-reference/keywords/value-types-table

            cannotBeExplicit = false;

            if (typeSymbol.IsAnonymousType && isExplicit)
            {
                // type cannot be expressed since it is anonymous, typeSymbol.ToDisplayString(format) wont work
                isExplicit = false;
            }

            switch (typeSymbol.SpecialType)
            {
                case SpecialType.System_Enum:
                    return SyntaxFactory.ParseExpression("(System.Enum)null")
                        .WithAdditionalAnnotations(Simplifier.Annotation);
                case SpecialType.System_ValueType:
                    return SyntaxFactory.ParseExpression("(System.ValueType)null")
                        .WithAdditionalAnnotations(Simplifier.Annotation);
                case SpecialType.System_Boolean:
                    return SyntaxFactory.ParseExpression("false");
                case SpecialType.System_Char:
                    return SyntaxFactory.ParseExpression("'\0'");
                case SpecialType.System_SByte:
                    return isExplicit ? SyntaxFactory.ParseExpression("(sbyte)0") : SyntaxFactory.ParseExpression("0");
                case SpecialType.System_Byte:
                    return isExplicit ? SyntaxFactory.ParseExpression("(byte)0") : SyntaxFactory.ParseExpression("0");
                case SpecialType.System_Int16:
                    return isExplicit ? SyntaxFactory.ParseExpression("(short)0") : SyntaxFactory.ParseExpression("0");
                case SpecialType.System_UInt16:
                    return isExplicit ? SyntaxFactory.ParseExpression("(ushort)0") : SyntaxFactory.ParseExpression("0");
                case SpecialType.System_Int32:
                    return isExplicit ? SyntaxFactory.ParseExpression("(int)0") : SyntaxFactory.ParseExpression("0");
                case SpecialType.System_UInt32:
                    return isExplicit ? SyntaxFactory.ParseExpression("(uint)0") : SyntaxFactory.ParseExpression("0u");
                case SpecialType.System_Int64:
                    return isExplicit ? SyntaxFactory.ParseExpression("(long)0") : SyntaxFactory.ParseExpression("0L");
                case SpecialType.System_UInt64:
                    return SyntaxFactory.ParseExpression("0ul");
                case SpecialType.System_Decimal:
                    return SyntaxFactory.ParseExpression("0m");
                case SpecialType.System_Single:
                    return SyntaxFactory.ParseExpression("0f");
                case SpecialType.System_Double:
                    return SyntaxFactory.ParseExpression("0d");
                case SpecialType.System_String:
                    return SyntaxFactory.ParseExpression("\"\"");
                    //return SyntaxFactory.ParseExpression("string.Empty");
                    //return SyntaxFactory.ParseExpression("null");
                case SpecialType.System_IntPtr:
                    return SyntaxFactory.ParseExpression("System.IntPtr.Zero")
                        .WithAdditionalAnnotations(Simplifier.Annotation);
                case SpecialType.System_UIntPtr:
                    return SyntaxFactory.ParseExpression("System.UIntPtr.Zero")
                        .WithAdditionalAnnotations(Simplifier.Annotation);
                case SpecialType.System_Nullable_T:
                    break;
                case SpecialType.System_DateTime:
                    return SyntaxFactory
                        .ParseExpression("System.DateTime.Now") // warning, this is not the default value, 0
                        .WithAdditionalAnnotations(Simplifier.Annotation);
                default:
                    break;
            }

            var isStruct = false;

            switch (typeSymbol.TypeKind)
            {
                case TypeKind.Enum:
                    var defaultEnumField = typeSymbol.GetMembers().FirstOrDefault(
                        m => m is IFieldSymbol mf && mf.HasConstantValue && Convert.ToInt64(mf.ConstantValue) == 0);
                    if (defaultEnumField != null)
                        return SyntaxFactory.ParseExpression(GetDisplayString(typeSymbol, out cannotBeExplicit) + "." +
                                                             defaultEnumField.Name)
                            .WithAdditionalAnnotations(Simplifier.Annotation);
                    else
                        return SyntaxFactory
                            .ParseExpression("(" + GetDisplayString(typeSymbol, out cannotBeExplicit) + ")0")
                            .WithAdditionalAnnotations(Simplifier.Annotation);
                case TypeKind.Error:
                    // throw new Exception();// can be uncommented when debugging
                    break;
                case TypeKind.Struct:
                    isStruct = true;
                    break;
                case TypeKind.TypeParameter:
                    //var typeParameterSymbol = typeSymbol as ITypeParameterSymbol;
                    if (typeSymbol.IsReferenceType)
                        break;
                    else if (typeSymbol.IsValueType)
                    {
                        isStruct = true;
                        break;
                    }
                    else
                        return SyntaxFactory
                            .ParseExpression("default(" + GetDisplayString(typeSymbol, out cannotBeExplicit) + ")")
                            .WithAdditionalAnnotations(Simplifier.Annotation);
                default:
                    break;
            }


            if (isStruct)
            {
                if (typeSymbol.IsTupleType)
                {
                    var named = typeSymbol as INamedTypeSymbol;
                    return SyntaxFactory.ParseExpression("(" +
                                                         string.Join(", ",
                                                             named.TupleElements.Select(te =>
                                                                 TypeToDefaultValue(te.Type, isExplicit)))
                                                         + ")");
                }
                else if (typeSymbol.NullableAnnotation == NullableAnnotation.Annotated)
                {
                    // do nothing
                }
                else
                {
                    return SyntaxFactory
                        .ParseExpression("new " + GetDisplayString(typeSymbol, out cannotBeExplicit) + "()")
                        .WithAdditionalAnnotations(Simplifier.Annotation);
                }
            }


            return isExplicit
                ? SyntaxFactory.ParseExpression("(" + GetDisplayString(typeSymbol, out cannotBeExplicit) + ")null")
                    .WithAdditionalAnnotations(Simplifier.Annotation)
                : SyntaxFactory.ParseExpression("null");

            string GetDisplayString(ITypeSymbol ts, out bool cantBeExplicit)
            {
                cantBeExplicit = false;

                var format = SymbolDisplayFormat.FullyQualifiedFormat
                    .WithGlobalNamespaceStyle(SymbolDisplayGlobalNamespaceStyle.OmittedAsContaining);
                var display = typeSymbol.ToDisplayString(format);

                // if an inner type is anonymous, disable explicit
                if (isExplicit && display.Contains("<anonymous type"))
                    cantBeExplicit = true;

                return display;
            }
        }
    }
}