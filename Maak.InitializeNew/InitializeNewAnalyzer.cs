using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace Maak.InitializeNew
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class InitializeNewAnalyzer : DiagnosticAnalyzer
    {
        // ReSharper disable once MemberCanBePrivate.Global
        public const string DiagnosticId = "MaakInitializeNew";

        // You can change these strings in the Resources.resx file. If you do not want your analyzer to be localize-able, you can use regular strings for Title and MessageFormat.
        // See https://github.com/dotnet/roslyn/blob/main/docs/analyzers/Localizing%20Analyzers.md for more on localization
        private static readonly LocalizableString Title = new LocalizableResourceString(nameof(Resources.AnalyzerTitle),
            Resources.ResourceManager, typeof(Resources));

        private static readonly LocalizableString MessageFormat =
            new LocalizableResourceString(nameof(Resources.AnalyzerMessageFormat), Resources.ResourceManager,
                typeof(Resources));

        private static readonly LocalizableString Description =
            new LocalizableResourceString(nameof(Resources.AnalyzerDescription), Resources.ResourceManager,
                typeof(Resources));

        private const string Category = "Usage";

        private static readonly DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat,
            Category, DiagnosticSeverity.Warning, isEnabledByDefault: true, description: Description);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Rule);

        public override void Initialize(AnalysisContext context)
        {
            // See https://github.com/dotnet/roslyn/blob/main/docs/analyzers/Analyzer%20Actions%20Semantics.md for more information
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
            context.EnableConcurrentExecution();
            context.RegisterSyntaxNodeAction(AnalyzeNode, SyntaxKind.LocalDeclarationStatement);
        }

        private static void AnalyzeNode(SyntaxNodeAnalysisContext context)
        {
            var localDeclaration = (LocalDeclarationStatementSyntax)context.Node;
            var typeDeclaration = localDeclaration.Declaration.Type;
            var symbolInfo = context.SemanticModel.GetSymbolInfo(typeDeclaration);
            var typeSymbol = symbolInfo.Symbol;
            
            if(typeSymbol == null)
                return;

            // Special case: Ensure that 'var' isn't actually an alias to another type. (e.g. using var = System.String).
            var aliasInfo = context.SemanticModel.GetAliasInfo(typeDeclaration);
            if (aliasInfo != null)
                return;

            var namedSymbol = context.Compilation.GetTypeByMetadataName(typeSymbol.MetadataName);

            if (namedSymbol?.TypeKind != TypeKind.Class)
                return;

            var hasDefaultConstructor = (namedSymbol?.Constructors)?.SingleOrDefault(c => !c.Parameters.Any()) != null;
            var properties = namedSymbol?.GetMembers()
                .Where(m => m.Kind == SymbolKind.Property
                            && m.DeclaredAccessibility == Accessibility.Public
                            && !((IPropertySymbol)m).IsReadOnly
                            && !((IPropertySymbol)m).IsStatic)
                .Select(m => new
                {
                    Name = m.Name,
                    Type = ((IPropertySymbol)m).Type
                })
                .ToList();
            var hasValidProperties = properties?.Any() != false;

            if (!hasValidProperties)
                return;
          
            var initializerExpressions = (localDeclaration.Declaration.Variables.FirstOrDefault()?.Initializer?.Value
                    as ObjectCreationExpressionSyntax)?.Initializer?.Expressions.ToList();
            
            // no initializer { } found and no default constructor
            if(initializerExpressions == null && !hasDefaultConstructor)
                return;

            var except = properties.Select(p => p.Name)
                .Except(initializerExpressions?.Select(e => (e as AssignmentExpressionSyntax)?.Left.ToString()) ?? new List<string>())
                .ToList();
            
            // all properties already exist in the initializer
            if(except.Count == 0)
                return;
            
            context.ReportDiagnostic(Diagnostic.Create(Rule, localDeclaration.Declaration.GetLocation()));
        }
    }
}