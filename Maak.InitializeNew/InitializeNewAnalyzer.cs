using System.Collections.Immutable;
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
        private static readonly LocalizableString Title = new LocalizableResourceString(nameof(Resources.AnalyzerTitle), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString MessageFormat = new LocalizableResourceString(nameof(Resources.AnalyzerMessageFormat), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString Description = new LocalizableResourceString(nameof(Resources.AnalyzerDescription), Resources.ResourceManager, typeof(Resources));
        private const string Category = "Usage";

        private static readonly DiagnosticDescriptor Rule = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, isEnabledByDefault: true, description: Description);

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

            // make sure the declaration isn't already const:
            if (localDeclaration.Modifiers.Any(SyntaxKind.ConstKeyword))
            {
                return;
            }

            var variableTypeName = localDeclaration.Declaration.Type;
            var variableType = context.SemanticModel.GetTypeInfo(variableTypeName, context.CancellationToken).ConvertedType;

            // Ensure that all variables in the local declaration have initializers that
            // are assigned with constant values.
            foreach (var initializer in localDeclaration.Declaration.Variables.Select(variable => variable.Initializer))
            {
                if (initializer == null)
                    return;

                var constantValue = context.SemanticModel.GetConstantValue(initializer.Value, context.CancellationToken);
                if (!constantValue.HasValue)
                    return;

                // Ensure that the initializer value can be converted to the type of the
                // local declaration without a user-defined conversion.
                if (variableType != null)
                {
                    var conversion = context.SemanticModel.ClassifyConversion(initializer.Value, variableType);
                    if (!conversion.Exists || conversion.IsUserDefined)
                        return;
                }

                // Special cases:
                //  * If the constant value is a string, the type of the local declaration
                //    must be System.String.
                //  * If the constant value is null, the type of the local declaration must
                //    be a reference type.
                if (constantValue.Value is string)
                {
                    if (variableType != null && variableType.SpecialType != SpecialType.System_String)
                        return;
                }
                else if (variableType != null && variableType.IsReferenceType && constantValue.Value != null)
                {
                    return;
                }
            }

            // Perform data flow analysis on the local declaration.
            var dataFlowAnalysis = context.SemanticModel.AnalyzeDataFlow(localDeclaration);

            if (localDeclaration.Declaration.Variables
                .Select(variable => context.SemanticModel.GetDeclaredSymbol(variable, context.CancellationToken))
                .Any(variableSymbol => dataFlowAnalysis != null && dataFlowAnalysis.WrittenOutside.Contains(variableSymbol)))
            {
                return;
            }

            context.ReportDiagnostic(Diagnostic.Create(Rule, context.Node.GetLocation()));
        }
    }
}
