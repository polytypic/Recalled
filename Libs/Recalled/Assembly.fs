namespace Recalled

open System.Reflection
open System.Runtime.InteropServices

[<AutoOpen>]
module AssemblyInfo =
  [<Literal>]
  let Version = Recalled.Internal.AssemblyInfo.Version

[<assembly: AssemblyTitle("Recalled")>]
[<assembly: AssemblyDescription("A library for defining persistent, incremental, parallel computations such as build systems.")>]
[<assembly: AssemblyConfiguration("")>]
[<assembly: AssemblyCompany("")>]
[<assembly: AssemblyProduct("Recalled")>]
[<assembly: AssemblyCopyright("© Vesa Karvonen")>]
[<assembly: AssemblyTrademark("")>]
[<assembly: AssemblyCulture("")>]

[<assembly: ComVisible(false)>]

[<assembly: Guid("214208bb-1b65-4dc6-b683-632f25e51e75")>]

[<assembly: AssemblyVersion(Version)>]
[<assembly: AssemblyFileVersion(Version)>]

()
