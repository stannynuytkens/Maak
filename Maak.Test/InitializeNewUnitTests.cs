using System.Threading.Tasks;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using VerifyCS = Maak.Test.CSharpCodeFixVerifier<
    Maak.InitializeNew.InitializeNewAnalyzer,
    Maak.CodeFixes.InitializeNewCodeFixProvider>;

namespace Maak.Test
{
    [TestClass]
    public class InitializeNewUnitTest
    {
        [TestMethod]
        public async Task OnlyPropertyIsAlreadyAssigned_NoDiagnostic()
        {
            await VerifyCS.VerifyAnalyzerAsync(@"
using System;

public class MyClass
{
    public int Age { get; set; }
    public float? Length { get; set; }
    public string Name { get; set; }
}

class Program
{
    static void Main()
    {
        var myClass = new MyClass
        {
            Age = 0,
            Length = null,
            Name = null
        };
    }
}
");
        }

        [TestMethod]
        public async Task NoPropertiesInitialized_Diagnostic()
        {
            await VerifyCS.VerifyCodeFixAsync(@"
using System;

public class MyClass
{
    public int Age { get; set; }
    public float? Length { get; set; }
    public string Name { get; set; }
}

class Program
{
    static void Main()
    {
        [|var myClass = new MyClass { }|];
    }
}
", @"
using System;

public class MyClass
{
    public int Age { get; set; }
    public float? Length { get; set; }
    public string Name { get; set; }
}

class Program
{
    static void Main()
    {
        var myClass = new MyClass
        {
            Name = null
        };
    }
}
");
        }
        
        [TestMethod]
        public async Task NoPropertiesInitializedNoInitializer_Diagnostic()
        {
            await VerifyCS.VerifyCodeFixAsync(@"
using System;

public class MyClass
{
    public int Age { get; set; }
    public float? Length { get; set; }
    public string Name { get; set; }
}

class Program
{
    static void Main()
    {
        [|var myClass = new MyClass()|];
    }
}
", @"
using System;

public class MyClass
{
    public int Age { get; set; }
    public float? Length { get; set; }
    public string Name { get; set; }
}

class Program
{
    static void Main()
    {
        var myClass = new MyClass
        {
            Name = null,
            Age = 0,
            Length = null
        };
    }
}
");
        }
    }
}
