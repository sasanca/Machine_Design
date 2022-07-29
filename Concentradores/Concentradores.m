(* ::Package:: *)

(* ::Text:: *)
(*(* :Title: Concentradores.m -- a package template *)*)
(**)
(*(* :Context: Concentradores` *)*)
(**)
(*(* :Author: Samuel S\[AAcute]nchez Caballero*)*)
(**)
(*(* :Summary:*)
(*   El paquete de FalloEstatico ha sido desarrollado para realizar los c\[AAcute]lculos est\[AAcute]ticos de elementos de m\[AAcute]quinas*)
(* *)*)
(**)
(*(* :Copyright: \[Copyright] <year> by <name or institution> *)*)
(**)
(*(* :Package Version: 0.0.1 *)*)
(**)
(*(* :Mathematica Version: 10.0 *)*)
(**)
(*(* :History:*)
(*   0.0.1 C\[AAcute]lculo de los principales concentradores de tensi\[OAcute]n.*)
(*   0.0.2 Se corrige la llamada a las funciones de muesca en eje que incorrectamente se denominaban ranura*)
(**)*)
(**)
(*(* :Keywords: template, fallo, estatico, package *)*)
(**)
(*(* :Sources:*)
(*   Samuel Sanchez-Caballero. C\[AAcute]lculo de elementos de maquinas, Createspace, 2017.*)
(**)*)
(**)
(*(* :Warnings:*)
(*   <description of global effects, incompatibilities>*)
(**)*)
(**)
(*(* :Limitations:*)
(*   <special cases not handled, known problems>*)
(**)*)
(**)
(*(* :Discussion:*)
(*   <description of algorithm, information for experts>*)
(**)*)
(**)
(*(* :Requirements:*)
(*   Concentradores/Concentradores.m*)
(**)*)
(**)
(*(* :Examples:*)
(*   <sample input that demonstrates the features of this package>*)
(**)*)
(**)
(**)
(*(* set up the package context, including public imports *)*)


BeginPackage["Concentradores`"]


KtPlanoTraccionCambio::usage="KtPlanoTraccionCambio[D,r,h] obtenemos el valor del concentrador de tension plano de un cambio de secci\[OAcute]n sometido a tracci\[OAcute]n";


KtPlanoFlexionCambio::usage="KtPlanoFlexionCambio[D,r,h] obtenemos el valor del concentrador de tension plano de un cambio de secci\[OAcute]n sometido a flexion";


KtPlanoTraccionMuesca::usage="KtPlanoTraccionMuesca[D,r,h] obtenemos el valor del concentrador de tension plano de una muesca sometida a tracci\[OAcute]n";


KtPlanoFlexionMuesca::usage="KtPlanoFlexionMuesca[D,r,h]obtenemos el valor del concentrador de tension plano de una muesca sometida a flexi\[OAcute]n";


KtPlanoTraccionAgujero::usage="KtPlanoTraccionAgujero[D,r] obtenemos el valor del concentrador de tension plano de un agujero sometido a tracci\[OAcute]n";


KtPlanoFlexionAgujero::usage="KtPlanoFlexionAgujero=2 tiene un valor constante para todas las geometr\[IAcute]as";


KtEjeTraccionCambio::usage="KtEjeTraccionCambio[D,r,h] obtenemos el valor del concentrador de tension para un eje con un cambio de secci\[OAcute]n sometido a tracci\[OAcute]n";


KtEjeFlexionCambio::usage="KtEjeFlexionCambio[D,r,h] obtenemos el valor del concentrador de tension para un eje con un cambio de secci\[OAcute]n sometido a flexi\[OAcute]n";


KtEjeTorsionCambio::usage="KtEjeTorsionCambio[D,r,h] obtenemos el valor del concentrador de tension para un eje con un cambio de secci\[OAcute]n sometido a torsion";


KtEjeTraccionMuesca::usage="KtEjeTraccionMuesca[D,r,h] obtenemos el valor del concentrador de tension para un eje con una ranuraa sometida sometido a tracci\[OAcute]n";


KtEjeFlexionMuesca::usage="KtEjeFlexionMuesca[D,r,h] obtenemos el valor del concentrador de tension para un eje con una ranuraa sometida sometido a flexi\[OAcute]n";


KtEjeTorsionMuesca::usage="KtEjeTorsionMuesca[D,r,h] obtenemos el valor del concentrador de tension para un eje con una ranuraa sometida sometido a torsi\[OAcute]n";


KtEjeTraccionAgujero::usage="KtEjeTraccionAgujero[D,r,h] obtenemos el valor del concentrador de tension para un eje con una agujero sometido sometido a tracci\[OAcute]n";


KtEjeFlexionAgujero::usage="KtEjeFlexionAgujero[D,r,h] obtenemos el valor del concentrador de tension para un eje con una agujero sometido sometido a tracci\[OAcute]n";


KtEjeTorsionAgujero::usage="KtEjeTorsionAgujero[D,r,h] obtenemos el valor del concentrador de tension para un eje con una agujero sometido sometido a tracci\[OAcute]n";


Begin["`Private`"]


(* ::Section:: *)
(*F\[OAcute]rmulas de uso general en el apartado*)


(* ::Text:: *)
(*Crea regla de sustitucion a partir de un array de valores*)


FS[lista_]:=Map[Function[x,Thread[{a,b,c}->x]],lista,{2}]


(* ::Text:: *)
(*F\[OAcute]rmula de calculo de las constantes C1, C2, C3 y C4 de la formulacion*)


FC[r_,h_]:= a + b Sqrt[h/r] + c h/r


(* ::Text:: *)
(*Regla de sustitucion con las constantes C1, C2, C3 y C4 de la formulacion en forma de lista*)


FC[r_,h_,FKt_]:=Map[Function[x,Thread[{C1,C2,C3,C4}->x]],(FC[r,h]/.FKt)]


(* ::Text:: *)
(*FC[r_, h_] := Array[Thread[{C1, C2, C3, C4} -> (FC[r, h] /.FC[r,h]/.FKt[[#]])] &, 2] (*Funcion equivalente con funciones puras*)*)


(* ::Text:: *)
(*F\[OAcute]rmula de c\[AAcute]lculo del valor de Kt*)


FK[D_,r_,h_]:=C1+C2(2h/D)+C3 (2h/D)^2+C4 (2h/D)^3


(* ::Text:: *)
(*Lista con los valores de Kt en funci\[OAcute]n del tramo de la funci\[OAcute]n (h/r > XX)*)


FK[D_,r_,h_,FKt_]:=FK[D,r,h]/.FC[r,h,FKt]


(* ::Section:: *)
(*Concentradores planos*)


(* ::Subchapter:: *)
(*Plano Traccion Cambio. (Fuente:Roark's Formulas for Stress and Strain, apartado 5a. p\[AAcute]gina 784)*)


FKtPlanoTraccionCambio:={
{
{+1.007,+1.000,-0.031},
{-0.114,-0.585,+0.314},
{+0.241,-0.992,-0.271},
{-0.134,+0.577,-0.012}
},
{
{+1.042,+0.982,-0.036},
{-0.074,-0.156,-0.010},
{-3.418,+1.220,-0.005},
{+3.450,-2.046,+0.051}
}
}


KtPlanoTraccionCambio[D_,r_,h_]:= Which[
h/r < 0.25, "No es valido para valores de h/r menores que 0.25",
h/r >= 0.25 && h/r <= 2 , FK[D,r,h,FS[FKtPlanoTraccionCambio]][[1]],
h/r > 2    && h/r < 20, FK[D,r,h,FS[FKtPlanoTraccionCambio]][[2]],
h/r > 20, "No es valido para valores de h/r mayores de 20"]


(*KtPlanoTraccionCambio[D_,r_,h_]:=If[h/r<0.1,"No es valido para valores de h/r menores que 0.1",If[h/r<=2.0,1.007+1*(h/r)^0.5-0.031*(h/r), If[h/r<=20.0,1.042+0.982*(h/r)^0.5-0.036*(h/r),If[h/r>20,"No es valido para valores de h/r mayores que 20"]]]]+(2*h/D)*If[h/r<0.1,"No es valido para valores de h/r menores que 0.1",If[h/r<=2.0,-0.114-0.585*(h/r)^0.5+0.314*(h/r), If[h/r<=20.0,-0.074+0.156*(h/r)^0.5-0.01*(h/r),If[h/r>20,"No es valido para valores de h/r mayores que 20"]]]]+(2*h/D)^2*If[h/r<0.1,"No es valido para valores de h/r menores que 0.1",If[h/r<=2.0,0.241-0.992*(h/r)^0.5-0.271*(h/r), If[h/r<=20.0,-3.418+1.22*(h/r)^0.5-0.005*(h/r),If[h/r>20,"No es valido para valores de h/r mayores que 20"]]]]+(2*h/D)^3*If[h/r<0.1,"No es valido para valores de h/r menores que 0.1",If[h/r<=2.0,-0.134+0.577*(h/r)^0.5-0.012*(h/r), If[h/r<=20.0,3.45-2.046*(h/r)^0.5+0.051*(h/r),If[h/r>20,"No es valido para valores de h/r mayores que 20"]]]]*)


(* ::Subchapter:: *)
(*Plano Flexion Cambio. (Fuente:Roark's Formulas for Stress and Strain, apartado 5b. p\[AAcute]gina 784)*)


FKtPlanoFlexionCambio:={
{
{+1.007,+1.000,-0.031},
{-0.270,-2.404,+0.749},
{+0.677,+1.133,-0.904},
{-0.414,+0.271,+0.186}
},
{
{+1.042,+0.982,-0.036},
{-3.599,+1.619,-0.431},
{+6.084,-5.607,+1.158},
{-2.527,+3.006,-0.691}
}
}


KtPlanoFlexionCambio[D_,r_,h_]:= Which[
h/r < 0.25, "No es valido para valores de h/r menores que 0.25",
h/r >= 0.25 && h/r <= 2 , FK[D,r,h,FS[FKtPlanoFlexionCambio]][[1]],
h/r >= 2    && h/r <= 20, FK[D,r,h,FS[FKtPlanoFlexionCambio]][[2]],
h/r > 20, "No es valido para valores de h/r mayores de 20"]


(*KtPlanoFlexionCambio[D_,r_,h_]:=If[h/r<0.1,"No es valido para valores de h/r menores que 0.1",If[h/r<=2.0,1.007+1*(h/r)^0.5-0.031*(h/r), If[h/r<=20.0,1.042+0.982*(h/r)^0.5-0.036*(h/r),If[h/r>20,"No es valido para valores de h/r mayores que 20"]]]]+(2*h/D)*If[h/r<0.1,"No es valido para valores de h/r menores que 0.1",If[h/r<=2.0,-0.27-2.404*(h/r)^0.5+0.749*(h/r), If[h/r<=20.0,-3.599+1.619*(h/r)^0.5-0.431*(h/r),If[h/r>20,"No es valido para valores de h/r mayores que 20"]]]]+(2*h/D)^2*If[h/r<0.1,"No es valido para valores de h/r menores que 0.1",If[h/r<=2.0,0.677+1.133*(h/r)^0.5-0.904*(h/r), If[h/r<=20.0,6.084-5.607*(h/r)^0.5+1.158*(h/r),If[h/r>20,"No es valido para valores de h/r mayores que 20"]]]]+(2*h/D)^3*If[h/r<0.1,"No es valido para valores de h/r menores que 0.1",If[h/r<=2.0,-0.414+0.271*(h/r)^0.5+0.186*(h/r), If[h/r<=20.0,-2.527+3.006*(h/r)^0.5-0.691*(h/r),If[h/r>20,"No es valido para valores de h/r mayores que 20"]]]]*)


(* ::Subchapter:: *)
(*Plano Traccion Muesca. (Fuente:Roark's Formulas for Stress and Strain, apartado 1a. p\[AAcute]gina 781)*)


FKtPlanoTraccionMuesca:={
{
{+0.850,+2.628,-0.413},
{-1.119,-4.826,+2.575},
{+3.563,-0.514,-2.402},
{-2.294,+2.713,+0.240}
},
{
{+0.833,+2.069,-0.009},
{+2.732,-4.157,+0.176},
{-8.859,+5.327,-0.320},
{+6.294,-3.239,+0.154}
}
}


KtPlanoTraccionMuesca[D_,r_,h_]:= Which[
h/r < 0.25, "No es valido para valores de h/r menores que 0.25",
h/r >= 0.25 && h/r <= 2 , FK[D,r,h,FS[FKtPlanoTraccionMuesca]][[1]],
h/r >= 2    && h/r <= 20, FK[D,r,h,FS[FKtPlanoTraccionMuesca]][[2]],
h/r > 50, "No es valido para valores de h/r mayores de 50"]


(*KtPlanoTraccionMuesca[D_,r_,h_]:=If[h/r<0.1,"No es valido para valores de h/r menores que 0.1",If[h/r<=2.0,0.85+2.628*(h/r)^0.5-0.413*(h/r), If[h/r<=50.0,0.833+2.069*(h/r)^0.5-0.009*(h/r),If[h/r>50,"No es valido para valores de h/r mayores que 50"]]]]+(2*h/D)*If[h/r<0.1,"No es valido para valores de h/r menores que 0.1",If[h/r<=2.0,-1.119-4.826*(h/r)^0.5+2.575*(h/r), If[h/r<=50.0,2.732-4.157*(h/r)^0.5+0.176*(h/r),If[h/r>50,"No es valido para valores de h/r mayores que 50"]]]]+(2*h/D)^2*If[h/r<0.1,"No es valido para valores de h/r menores que 0.1",If[h/r<=2.0,3.563-0.514*(h/r)^0.5-2.402*(h/r), If[h/r<=50.0,-8.859+5.327*(h/r)^0.5-0.32*(h/r),If[h/r>50,"No es valido para valores de h/r mayores que 50"]]]]+(2*h/D)^3*If[h/r<0.1,"No es valido para valores de h/r menores que 0.1",If[h/r<=2.0,-2.294+2.713*(h/r)^0.5+0.24*(h/r), If[h/r<=50.0,6.294-3.239*(h/r)^0.5+0.154*(h/r),If[h/r>50,"No es valido para valores de h/r mayores que 50"]]]]*)


(* ::Subchapter:: *)
(*Plano Flexion Muesca. (Fuente:Roark's Formulas for Stress and Strain, apartado 1b. p\[AAcute]gina 782)*)


FKtPlanoFlexionMuesca:={
{
{+0.723,+2.845,-0.504},
{-1.836,-5.746,+1.314},
{+7.254,-1.885,+1.646},
{-5.140,+4.785,-2.456}
},
{
{+0.833,+2.069,-0.009},
{+0.024,-5.383,+0.126},
{-0.856,+6.460,-0.199},
{+0.999,-3.146,+0.082}
}
}


KtPlanoFlexionMuesca[D_,r_,h_]:= Which[
h/r < 0.25, "No es valido para valores de h/r menores que 0.25",
h/r >= 0.25 && h/r <= 2 , FK[D,r,h,FS[FKtPlanoFlexionMuesca]][[1]],
h/r >= 2    && h/r <= 20, FK[D,r,h,FS[FKtPlanoFlexionMuesca]][[2]],
h/r > 50, "No es valido para valores de h/r mayores de 50"]


(*KtPlanoFlexionMuesca[D_,r_,h_]:=If[h/r<0.1,"No es valido para valores de h/r menores que 0.1",If[h/r<=2.0,0.723+2.845*(h/r)^0.5-0.504*(h/r), If[h/r<=50.0,0.833+2.069*(h/r)^0.5-0.009*(h/r),If[h/r>50,"No es valido para valores de h/r mayores que 50"]]]]+(2*h/D)*If[h/r<0.1,"No es valido para valores de h/r menores que 0.1",If[h/r<=2.0,-1.836-5.746*(h/r)^0.5+1.314*(h/r), If[h/r<=50.0,0.024-5.383*(h/r)^0.5+0.126*(h/r),If[h/r>50,"No es valido para valores de h/r mayores que 50"]]]]+(2*h/D)^2*If[h/r<0.1,"No es valido para valores de h/r menores que 0.1",If[h/r<=2.0,7.254-1.885*(h/r)^0.5+1.646*(h/r), If[h/r<=50.0,-0.856+6.46*(h/r)^0.5-0.199*(h/r),If[h/r>50,"No es valido para valores de h/r mayores que 50"]]]]+(2*h/D)^3*If[h/r<0.1,"No es valido para valores de h/r menores que 0.1",If[h/r<=2.0,-5.14+4.785*(h/r)^0.5-2.456*(h/r), If[h/r<=50.0,0.999-3.146*(h/r)^0.5+0.082*(h/r),If[h/r>50,"No es valido para valores de h/r mayores que 50"]]]]*)


(* ::Subchapter:: *)
(*Plano Traccion Agujero*)


KtPlanoTraccionAgujero[D_,r_]:=3-3.13*(2r/D)+3.66*(2r/D)^2-1.53*(2r/D)^3


(* ::Subchapter:: *)
(*Plano Flexion Agujero*)


KtPlanoFlexionAgujero:=2


(* ::Section:: *)
(*Concentradores ejes*)


(* ::Subchapter:: *)
(*Eje Traccion Cambio. (Fuente:Roark's Formulas for Stress and Strain, apartado 17a. p\[AAcute]gina 791*)


FKtEjeTraccionCambio:={
{
{+0.927, +1.149, -0.086},
{+0.011, -3.029, +0.948}, 
{-0.304, +3.979, -1.737},
{+0.366, -2.098, +0.875}
},
{
{+1.225, +0.831, -0.010},
{-1.831, -0.318, -0.049},
{+2.236, -0.522, +0.176},
{-0.630, +0.009, -0.117}
}
}


KtEjeTraccionCambio[D_,r_,h_]:= Which[
h/r < 0.25, "No es valido para valores de h/r menores que 0.25",
h/r >= 0.25 && h/r <= 2 , FK[D,r,h,FS[FKtEjeTraccionCambio]][[1]],
h/r >= 2    && h/r <= 20, FK[D,r,h,FS[FKtEjeTraccionCambio]][[2]],
h/r > 20, "No es valido para valores de h/r mayores de 20"]


(*KtEjeTraccionCambio[D_,r_,h_]:=If[h/r<0.25,"No es valido para valores de h/r menores que 0.25",If[h/r<=2.0,0.927+1.149*(h/r)^0.5-0.086*(h/r), If[h/r<=20.0,1.225+0.831*(h/r)^0.5-0.01*(h/r),If[h/r>20,"No es valido para valores de h/r mayores que 20"]]]]+(2*h/D)*If[h/r<0.25,"No es valido para valores de h/r menores que 0.25",If[h/r<=2.0,0.011-3.029*(h/r)^0.5+0.948*(h/r), If[h/r<=20.0,-1.831-0.318*(h/r)^0.5-0.049*(h/r),If[h/r>20,"No es valido para valores de h/r mayores que 20"]]]]+(2*h/D)^2*If[h/r<0.25,"No es valido para valores de h/r menores que 0.25",If[h/r<=2.0,-0.304+3.979*(h/r)^0.5-1.737*(h/r), If[h/r<=20.0,2.236-0.522*(h/r)^0.5+0.176*(h/r),If[h/r>20,"No es valido para valores de h/r mayores que 20"]]]]+(2*h/D)^3*If[h/r<0.25,"No es valido para valores de h/r menores que 0.25",If[h/r<=2.0,0.366-2.098*(h/r)^0.5+0.875*(h/r), If[h/r<=20.0,-0.63+0.009*(h/r)^0.5-0.117*(h/r),If[h/r>20,"No es valido para valores de h/r mayores que 20"]]]]*)


(* ::Subchapter:: *)
(*Eje Flexion Cambio. (Fuente:Roark's Formulas for Stress and Strain, apartado 17b. p\[AAcute]gina 791*)


FKtEjeFlexionCambio:={
{
{0.927,1.149,-0.086},
{0.015,-3.281,0.837},
{0.847,1.716,-0.506},
{-0.79,0.417,-0.246}
},
{
{1.255,0.831,-0.01},
{-3.79,0.958,-0.257},
{7.374,-4.834,0.862},
{-3.809,3.046,-0.595}
}
}


KtEjeFlexionCambio[D_,r_,h_]:= Which[
h/r < 0.25, "No es valido para valores de h/r menores que 0.25",
h/r >= 0.25 && h/r <= 2 , FK[D,r,h,FS[FKtEjeFlexionCambio]][[1]],
h/r >= 2    && h/r <= 20, FK[D,r,h,FS[FKtEjeFlexionCambio]][[2]],
h/r > 20, "No es valido para valores de h/r mayores de 20"]


(*KtEjeFlexionCambio[D_,r_,h_]:=If[h/r<0.25,"No es valido para valores de h/r menores que 0.25",If[h/r<=2.0,0.927+1.149*(h/r)^0.5-0.086*(h/r), If[h/r<=20.0,1.225+0.831*(h/r)^0.5-0.01*(h/r),If[h/r>20,"No es valido para valores de h/r mayores que 20"]]]]+(2*h/D)*If[h/r<0.25,"No es valido para valores de h/r menores que 0.25",If[h/r<=2.0,0.015-3.281*(h/r)^0.5-0.837*(h/r), If[h/r<=20.0,-3.79+0.958*(h/r)^0.5-0.257*(h/r),If[h/r>20,"No es valido para valores de h/r mayores que 20"]]]]+(2*h/D)^2*If[h/r<0.25,"No es valido para valores de h/r menores que 0.25",If[h/r<=2.0,0.847+1.716*(h/r)^0.5-0.506*(h/r), If[h/r<=20.0,7.374-4.834*(h/r)^0.5+0.862*(h/r),If[h/r>20,"No es valido para valores de h/r mayores que 20"]]]]+(2*h/D)^3*If[h/r<0.25,"No es valido para valores de h/r menores que 0.25",If[h/r<=2.0,-0.79+0.417*(h/r)^0.5-0.246*(h/r), If[h/r<=20.0,-3.809+3.046*(h/r)^0.5-0.595*(h/r),If[h/r>20,"No es valido para valores de h/r mayores que 20"]]]]*)


(* ::Subchapter:: *)
(*Eje Torsion Cambio. (Fuente:Roark's Formulas for Stress and Strain, apartado 17c. p\[AAcute]gina 791*)


FKtEjeTorsionCambio:={
{
{+0.953, +0.680, -0.053},
{-0.493, -1.820, +0.517},
{+1.621, +0.908, -0.529},
{-1.081, +0.232, +0.065}
}
}


KtEjeTorsionCambio[D_,r_,h_]:= Which[
h/r < 0.25, "No es valido para valores de h/r menores que 0.25",
h/r >= 0.25 && h/r <= 4 , FK[D,r,h,FS[FKtEjeTorsionCambio]][[1]],
h/r > 4, "No es valido para valores de h/r mayores de 4"]


(*KtEjeTorsionCambio[D_,r_,h_]:=If[h/r<0.25,"No es valido para valores de h/r menores que 0.25",If[h/r<=4.0,0.953+0.68*(h/r)^0.5-0.053*(h/r),If[h/r>4,"No es valido para valores de h/r mayores que 4"]]]+(2*h/D)*If[h/r<0.25,"No es valido para valores de h/r menores que 0.25",If[h/r<=4.0,-0.493-1.82*(h/r)^0.5+0.517*(h/r), If[h/r>4,"No es valido para valores de h/r mayores que 4"]]]+(2*h/D)^2*If[h/r<0.25,"No es valido para valores de h/r menores que 0.25",If[h/r<=4.0,1.621+0.908*(h/r)^0.5-0.529*(h/r), If[h/r>4,"No es valido para valores de h/r mayores que 4"]]]+(2*h/D)^3*If[h/r<0.25,"No es valido para valores de h/r menores que 0.25",If[h/r<=4.0,-1.081+0.232*(h/r)^0.5+0.065*(h/r), If[h/r>4,"No es valido para valores de h/r mayores que 4"]]]*)


(* ::Subchapter:: *)
(*Eje Traccion Muesca. (Fuente:Roark's Formulas for Stress and Strain, apartado 15a. p\[AAcute]gina 790*)


FKtEjeTraccionMuesca:={
{
{0.455,+3.354,-0.769},
{3.129,-15.955,+7.404},
{-6.909,+29.286,-16.104},
{4.325,-16.685,+9.469}
},
{
{+0.935,+1.922,+0.004},
{+0.537,-3.708,+0.040},
{-2.538,+3.438,-0.012},
{+2.066,-1.652,-0.031}
}
}


KtEjeTraccionMuesca[D_,r_,h_]:= Which[
h/r < 0.25, "No es valido para valores de h/r menores que 0.25",
h/r >= 0.25 && h/r <= 2 , FK[D,r,h,FS[FKtEjeTraccionMuesca]][[1]],
h/r >= 2    && h/r <= 20, FK[D,r,h,FS[FKtEjeTraccionMuesca]][[2]],
h/r > 50, "No es valido para valores de h/r mayores de 50"]


(*KtEjeTraccionMuesca[D_,r_,h_]:=If[h/r<0.25,"No es valido para valores de h/r menores que 0.25",If[h/r<=2.0,0.455+3.354*(h/r)^0.5-0.769*(h/r), If[h/r<=50.0,0.935+1.922*(h/r)^0.5-0.004*(h/r),If[h/r>50,"No es valido para valores de h/r mayores que 50"]]]]+(2*h/D)*If[h/r<0.25,"No es valido para valores de h/r menores que 0.25",If[h/r<=2.0,3.129-15.955*(h/r)^0.5+7.404*(h/r), If[h/r<=50.0,0.537-3.708*(h/r)^0.5-0.04*(h/r),If[h/r>50,"No es valido para valores de h/r mayores que 50"]]]]+(2*h/D)^2*If[h/r<0.25,"No es valido para valores de h/r menores que 0.25",If[h/r<=2.0,-6.909+29.286*(h/r)^0.5-16.104*(h/r), If[h/r<=50.0,-2.538+3.438*(h/r)^0.5+0.012*(h/r),If[h/r>50,"No es valido para valores de h/r mayores que 50"]]]]+(2*h/D)^3*If[h/r<0.25,"No es valido para valores de h/r menores que 0.25",If[h/r<=2.0,4.325-16.685*(h/r)^0.5+9.469*(h/r), If[h/r<=50.0,2.066-1.652*(h/r)^0.5-0.031*(h/r),If[h/r>50,"No es valido para valores de h/r mayores que 50"]]]]*)


(* ::Subchapter:: *)
(*Eje Flexion Muesca. (Fuente:Roark's Formulas for Stress and Strain, apartado 15b. p\[AAcute]gina 790*)


FKtEjeFlexionMuesca:={
{
{+0.455,+3.354,-0.769},
{+0.891,-12.721,+4.593},
{+0.286,+15.481,-6.392},
{-0.632,-6.115,+2.568}
},
{
{+0.935,+1.922,+0.004},
{-0.552,-5.327,+0.086},
{+0.754,+6.281,-0.121},
{-0.138,-2.876,+0.031}
}
}


KtEjeFlexionMuesca[D_,r_,h_]:= Which[
h/r < 0.25, "No es valido para valores de h/r menores que 0.25",
h/r >= 0.25 && h/r <= 2 , FK[D,r,h,FS[FKtEjeFlexionMuesca]][[1]],
h/r >= 2    && h/r <= 50, FK[D,r,h,FS[FKtEjeFlexionMuesca]][[2]],
h/r > 50, "No es valido para valores de h/r mayores de 50"]


(*KtEjeFlexionMuesca[D_,r_,h_]:=If[h/r<0.25,"No es valido para valores de h/r menores que 0.25",If[h/r<=2.0,0.455+3.354*(h/r)^0.5-0.769*(h/r), If[h/r<=50.0,0.935+1.922*(h/r)^0.5+0.004*(h/r),If[h/r>50,"No es valido para valores de h/r mayores que 50"]]]]+(2*h/D)*If[h/r<0.25,"No es valido para valores de h/r menores que 0.25",If[h/r<=2.0,0.891-12.721*(h/r)^0.5+4.593*(h/r), If[h/r<=50.0,-0.552-5.327*(h/r)^0.5+0.086*(h/r),If[h/r>50,"No es valido para valores de h/r mayores que 50"]]]]+(2*h/D)^2*If[h/r<0.25,"No es valido para valores de h/r menores que 0.25",If[h/r<=2.0,0.286+15.481*(h/r)^0.5-6.392*(h/r), If[h/r<=50.0,0.754+6.281*(h/r)^0.5-0.121*(h/r),If[h/r>50,"No es valido para valores de h/r mayores que 50"]]]]+(2*h/D)^3*If[h/r<0.25,"No es valido para valores de h/r menores que 0.25",If[h/r<=2.0,-0.632-6.115*(h/r)^0.5+2.568*(h/r), If[h/r<=50.0,-0.138-2.876*(h/r)^0.5+0.031*(h/r),If[h/r>50,"No es valido para valores de h/r mayores que 50"]]]]*)


(* ::Subchapter:: *)
(*Eje Torsion Muesca.(Fuente:Roark's Formulas for Stress and Strain, apartado 15c. p\[AAcute]gina 790*)


FKtEjeTorsionMuesca:=Map[Function[x,Thread[{a,b,c}->x]],{
{
{+1.245,+0.264,+0.491},
{-3.030,+3.269,-3.633},
{+7.199,-11.286,+8.318},
{-4.414,+7.753,-5.176}
},
{
{+1.651,+0.614,+0.040},
{-4.794,-0.314,-0.217},
{+8.457,-0.962,+0.389},
{-4.314,+0.662,-0.212}
}
},{2}]


KtEjeTorsionMuesca[D_,r_,h_]:= Which[
h/r < 0.25, "No es valido para valores de h/r menores que 0.25",
h/r >= 0.25 && h/r <= 2 , FK[D,r,h,FKtEjeTorsionMuesca][[1]],
h/r >= 2    && h/r <= 50, FK[D,r,h,FKtEjeTorsionMuesca][[2]],
h/r > 50, "No es valido para valores de h/r mayores de 50"]


(*KtEjeTorsionMuesca[D_,r_,h_]:=If[h/r<0.25,"No es valido para valores de h/r menores que 0.25",If[h/r<=2.0,1.245+0.264*(h/r)^0.5+0.491*(h/r), If[h/r<=50.0,1.651+0.614*(h/r)^0.5+0.004*(h/r),If[h/r>50,"No es valido para valores de h/r mayores que 50"]]]]+(2*h/D)*If[h/r<0.25,"No es valido para valores de h/r menores que 0.25",If[h/r<=2.0,-3.03+3.269*(h/r)^0.5-3.633*(h/r), If[h/r<=50.0,-4.794-0.314*(h/r)^0.5-0.217*(h/r),If[h/r>50,"No es valido para valores de h/r mayores que 50"]]]]+(2*h/D)^2*If[h/r<0.25,"No es valido para valores de h/r menores que 0.25",If[h/r<=2.0,7.199-11.286*(h/r)^0.5+8.318*(h/r), If[h/r<=50.0,8.457-0.962*(h/r)^0.5+0.389*(h/r),If[h/r>50,"No es valido para valores de h/r mayores que 50"]]]]+(2*h/D)^3*If[h/r<0.25,"No es valido para valores de h/r menores que 0.25",If[h/r<=2.0,-4.414+7.753*(h/r)^0.5-5.176*(h/r), If[h/r<=50.0,-4.314+0.662*(h/r)^0.5-0.212*(h/r),If[h/r>50,"No es valido para valores de h/r mayores que 50"]]]]*)


(* ::Subchapter:: *)
(*Eje Traccion Agujero. (Fuente:Roark's Formulas for Stress and Strain, apartado 18a. p\[AAcute]gina 792*)


FKtEjeTraccionAgujero:={
{
{+3.000},
{+2.773,+1.529,-4.379},
{-0.421,-12.782,+22.781},
{+16.841,+16.678,-40.007}
}
}


KtEjeTraccionAgujero[D_,d_,r_]:= Which[
d/D < 0.9 && 2r/D < 0.45 , FK[D,d,r,FS[FKtEjeTraccionAgujero]][[1]],
d/D > 0.9, "No es valido para valores de d/D mayores de 0.9",
2r/D > 0.45 "No es v\[AAcute]lido para valores 2r/D mayores de 0.45"]


(* ::Subchapter:: *)
(*Eje Flexi\[OAcute]n Agujero. (Fuente:Roark's Formulas for Stress and Strain, apartado 18b. p\[AAcute]gina 792*)


FKtEjeFlexionAgujero:={
{
{+3.000},
{-6.690,-1.620,+4.432},
{+44.739,+10.724,-19.927},
{-53.307,-25.998,+43.258}
}
}


KtEjeFlexionAgujero[D_,d_,r_]:= Which[
d/D <= 0.9 && 2r/D <= 0.3 , FK[D,d,r,FS[FKtEjeFlexionAgujero]][[1]],
d/D > 0.9, "No es valido para valores de d/D mayores de 0.9",
2r/D > 0.3 "No es v\[AAcute]lido para valores 2r/D mayores de 0.3"]


(* ::Subchapter:: *)
(*Eje Torsi\[OAcute]n Agujero. (Fuente:Roark's Formulas for Stress and Strain, apartado 18c. p\[AAcute]gina 792*)


FKtEjeTorsionAgujero:={
{
{+4.000},
{-6.793,+1.133,-0.126},
{+38.382,-7.242,+6.495},
{-44.576,-7.428,+58.656}
}
}


KtEjeTorsionAgujero[D_,d_,r_]:= Which[
d/D <= 0.9 && 2r/D <= 0.3 , FK[D,d,r,FS[FKtEjeTorsionAgujero]][[1]],
d/D > 0.9, "No es valido para valores de d/D mayores de 0.9",
2r/D > 0.4 "No es v\[AAcute]lido para valores 2r/D mayores de 0.4"]


End[ ]
EndPackage[ ]
