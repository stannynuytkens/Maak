﻿using System.Diagnostics.CodeAnalysis;

namespace Maak.CodeFixes.ModelBuilder
{
    [ExcludeFromCodeCoverage]
    public static class StringExtension
    {
        public static string ToCamelCase(this string str)
        {
            if (!string.IsNullOrEmpty(str) && str.Length > 1)
            {
                return char.ToLowerInvariant(str[0]) + str.Substring(1);
            }
            return str;
        }

        public static string ToPascalCase(this string str)
        {
            if (!string.IsNullOrEmpty(str) && str.Length > 1)
            {
                return char.ToUpperInvariant(str[0]) + str.Substring(1);
            }
            return str;
        }
    }
}