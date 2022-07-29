Expand-Archive Concentradores.zip -DestinationPath C:\ProgramData\Mathematica\Applications -Force
Expand-Archive FalloEstatico.zip -DestinationPath C:\ProgramData\Mathematica\Applications -Force
Expand-Archive FalloFatiga.zip -DestinationPath C:\ProgramData\Mathematica\Applications -Force
Copy-Item -Path .\Test.nb -Destination C:\ProgramData\Mathematica\Applications -Force
# & 'C:\Program Files\Wolfram Research\Mathematica\11.3\Mathematica.exe' -run C:\ProgramData\Mathematica\Applications\Test.nb
