namespace Recalled.Internal

open System.Reflection
open System.Runtime.InteropServices

[<AutoOpen>]
module AssemblyInfo =
  [<Literal>]
  let Version = "0.0.0.6"

[<assembly: AssemblyTitle("Recalled.Internal")>]
[<assembly: AssemblyDescription("Internal modules used by the Recalled library.")>]
[<assembly: AssemblyConfiguration("")>]
[<assembly: AssemblyCompany("")>]
[<assembly: AssemblyProduct("Recalled.Internal")>]
[<assembly: AssemblyCopyright("© Vesa Karvonen")>]
[<assembly: AssemblyTrademark("")>]
[<assembly: AssemblyCulture("")>]

[<assembly: ComVisible(false)>]

[<assembly: Guid("a1cc088b-03a7-449e-bc3d-235cf6a6bb83")>]

[<assembly: AssemblyVersion(Version)>]
[<assembly: AssemblyFileVersion(Version)>]

()
