using System.Collections.Generic;
using Microsoft.CodeAnalysis;

namespace Maak.CodeFixes.ModelBuilder
{
    public class TypeDefinition
    {
        public string Name { get; set; }
        public string Namespace { get; set; }
        public string Assembly { get; set; }
        public List<IPropertySymbol> Properties { get; set; }
        public override string ToString() => $"{Namespace}.{Name}";
    }
}