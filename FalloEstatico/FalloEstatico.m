(* ::Package:: *)

(* ::Text:: *)
(*(* :Title: FalloEstatico.m -- a package template *)*)
(**)
(*(* :Context: FalloEstatico` *)*)
(**)
(*(* :Author: Samuel S\[AAcute]nchez Caballero*)*)
(**)
(*(* :Summary:*)
(*   El paquete de FalloEstatico ha sido desarrollado para realizar los c\[AAcute]lculos est\[AAcute]ticos de elementos de m\[AAcute]quinas*)
(* *)*)
(**)
(*(* :Copyright: \[Copyright] <year> by <name or institution> *)*)
(**)
(*(* :Package Version: 0.0.2 *)*)
(**)
(*(* :Mathematica Version: 10.0 *)*)
(**)
(*(* :History:*)
(*   0.0.2 Fallo estatico. Corrige errores en ayuda contextual teor\[IAcute]a de Von Mises y Dowling.*)
(*   0.0.1 Fallo estatico.*)
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
(*   FalloEstatico/Falloestatico.m*)
(**)*)
(**)
(*(* :Examples:*)
(*   <sample input that demonstrates the features of this package>*)
(**)*)
(**)
(**)
(*(* set up the package context, including public imports *)*)


BeginPackage["FalloEstatico`"]


Principales::usage="Principales[\[Sigma]x,\[Sigma]y,\[Sigma]z,\[Tau]xy,\[Tau]xz,\[Tau]yz] calcula las tensiones principales mediante la diagonalizaci\[OAcute]n del tensor de tensiones";


Tresca::usage="Tresca[\[Sigma]x,\[Sigma]y,\[Sigma]z,\[Tau]xy,\[Tau]xz,\[Tau]yz] calcula la tensi\[OAcute]n equivalente para materiales d\[UAcute]ctiles seg\[UAcute]n la teor\[IAcute]a de Tresca, a partir de las seis tensiones espaciales..
Tresca[\[Sigma]1,\[Sigma]2,\[Sigma]3] calcula la tensi\[OAcute]n equivalente para materiales d\[UAcute]ctiles seg\[UAcute]n la teor\[IAcute]a de Tresca, a partir de las tres tensiones principales";


Mises::usage="Mises[\[Sigma]x,\[Sigma]y,\[Sigma]z,\[Tau]xy,\[Tau]xz,\[Tau]yz] calcula la tensi\[OAcute]n equivalente para materiales d\[UAcute]ctiles seg\[UAcute]n la teor\[IAcute]a de Von Mises, a partir de las seis tensiones espaciales.
Mises[\[Sigma]1,\[Sigma]2,\[Sigma]3] calcula la tensi\[OAcute]n equivalente para materiales d\[UAcute]ctiles seg\[UAcute]n la teor\[IAcute]a de Von Mises, a partir de las tres tensiones principales.";


Dowling::usage="Dowling[\[Sigma]x,\[Sigma]y,\[Sigma]z,\[Tau]xy,\[Tau]xz,\[Tau]yz,Sut,Suc] calcula la tensi\[OAcute]n equivalente para materiales fr\[AAcute]giles seg\[UAcute]n la teor\[IAcute]a de Mohr modificada, empleando las ecuaciones de Dowling,a partir de las seis tensiones espaciales.
Dowling[\[Sigma]1,\[Sigma]2,\[Sigma]3,Sut,Suc] calcula la tensi\[OAcute]n equivalente para materiales fr\[AAcute]giles seg\[UAcute]n la teor\[IAcute]a de Mohr modificada, empleando las ecuaciones de Dowling, a partir de las tres tensiones principales.";


SigmaEjeTraccion::usage="SigmaEjeTraccion[P,d] calcula la tensi\[OAcute]n normal de un eje circular sometido a tracci\[OAcute]n";


SigmaEjeFlexion::usage="SigmaEjeFlexion[M,d] calcula la tensi\[OAcute]n normal de un eje circular sometido a flexi\[OAcute]n";


TauEjeFlexion::usage="TauEjeFlexion[V,d] calcula la tensi\[OAcute]n cortante (Collignon) de un eje circular sometido a flexi\[OAcute]n";


TauEjeTorsion::usage="TauEjeTorsion[T,d] calcula la tensi\[OAcute]n cortante de un eje circular sometido a torsi\[OAcute]n";


Begin["`Private`"]


(* ::Title:: *)
(*Formulario Cargas Constantes*)


(* ::Subsection:: *)
(*C\[AAcute]lculo de las tensiones principales en funci\[OAcute]n de las componentes de tensi\[OAcute]n*)


Principales[\[Sigma]x_, \[Sigma]y_, \[Sigma]z_, \[Tau]xy_, \[Tau]xz_, \[Tau]yz_] :=  Eigenvalues[{{\[Sigma]x, \[Tau]xy, \[Tau]xz}, {\[Tau]xy, \[Sigma]y, \[Tau]yz}, {\[Tau]xz, \[Tau]yz, \[Sigma]z}}]


(* ::Subsection:: *)
(*C\[AAcute]lculo de la tensi\[OAcute]n equivalente de Tresca*)


(* ::Subsubsection:: *)
(*En funci\[OAcute]n de las tensiones principales*)


Tresca[\[Sigma]1_,\[Sigma]2_,\[Sigma]3_]:=1/2(Abs[\[Sigma]1-\[Sigma]3]+Abs[\[Sigma]1-\[Sigma]2]+Abs[\[Sigma]2-\[Sigma]3])


(* ::Subsubsection:: *)
(*En funci\[OAcute]n del vector de tensiones principales*)


Tresca[{\[Sigma]1_,\[Sigma]2_,\[Sigma]3_}]:=Tresca[\[Sigma]1,\[Sigma]2,\[Sigma]3]


(* ::Subsubsection:: *)
(*En funci\[OAcute]n de las seis componentes de tension en el espacio*)


Tresca[\[Sigma]x_,\[Sigma]y_,\[Sigma]z_,\[Tau]xy_,\[Tau]xz_,\[Tau]yz_]:=Tresca[Principales[\[Sigma]x,\[Sigma]y,\[Sigma]z,\[Tau]xy,\[Tau]xz,\[Tau]yz]]


(* ::Subsection:: *)
(*C\[AAcute]lculo de la tensi\[OAcute]n equivalente de Von Mises*)


(* ::Subsubsection:: *)
(*En funci\[OAcute]n de las tensiones principales*)


Mises[\[Sigma]1_,\[Sigma]2_,\[Sigma]3_]:=Sqrt[1/2((\[Sigma]1-\[Sigma]2)^2+(\[Sigma]1-\[Sigma]3)^2+(\[Sigma]2-\[Sigma]3)^2)]


(* ::Subsubsection:: *)
(*En funci\[OAcute]n del vector de tensiones principales*)


Mises[{\[Sigma]1_,\[Sigma]2_,\[Sigma]3_}]:=Mises[\[Sigma]1,\[Sigma]2,\[Sigma]3]


(* ::Subsubsection:: *)
(*En funci\[OAcute]n de las seis componentes de tension en el espacio*)


Mises[\[Sigma]x_,\[Sigma]y_,\[Sigma]z_,\[Tau]xy_,\[Tau]xz_,\[Tau]yz_]:=Mises[Principales[\[Sigma]x,\[Sigma]y,\[Sigma]z,\[Tau]xy,\[Tau]xz,\[Tau]yz]]


(* ::Subsection:: *)
(*C\[AAcute]lculo de la tensi\[OAcute]n equivalente de Coulomb-Mohr modificada (m\[EAcute]todo de Dowling)*)


(* ::Subsubsection:: *)
(*C\[AAcute]lculo de las constantes de Dowling*)


CDowling[\[Sigma]a_,\[Sigma]b_,Sut_,Suc_]:=1/2(Abs[\[Sigma]a-\[Sigma]b]+(1-2Sut/Suc)(\[Sigma]a+\[Sigma]b))


(* ::Subsubsection:: *)
(*En funci\[OAcute]n de las tensiones principales*)


Dowling[\[Sigma]1_,\[Sigma]2_,\[Sigma]3_,Sut_,Suc_]:=Max[\[Sigma]1,\[Sigma]2,\[Sigma]3,CDowling[\[Sigma]1,\[Sigma]2,Sut,Suc],CDowling[\[Sigma]1,\[Sigma]3,Sut,Suc],CDowling[\[Sigma]2,\[Sigma]3,Sut,Suc]]


(* ::Subsubsection:: *)
(*En funci\[OAcute]n del vector de tensiones principales*)


Dowling[{\[Sigma]1_,\[Sigma]2_,\[Sigma]3_},Sut_,Suc_]:=Dowling[\[Sigma]1,\[Sigma]2,\[Sigma]3,Sut,Suc]


(* ::Subsubsection:: *)
(*En funci\[OAcute]n de las seis componentes de tension en el espacio*)


Dowling[\[Sigma]x_,\[Sigma]y_,\[Sigma]z_,\[Tau]xy_,\[Tau]xz_,\[Tau]yz_,Sut_,Suc_]:=Dowling[Principales[\[Sigma]x,\[Sigma]y,\[Sigma]z,\[Tau]xy,\[Tau]xz,\[Tau]yz],Sut,Suc]


(* ::Subsection:: *)
(*C\[AAcute]lculo de tensiones normales y cortantes*)


(* ::Subsubsection:: *)
(*Tensiones en ejes de secci\[OAcute]n circular*)


SigmaEjeTraccion[P_,d_]:=(4 P)/(d^2 \[Pi])


SigmaEjeFlexion[M_,d_]:=(32 M)/(\[Pi] d^3)


TauEjeFlexion[V_,d_]:=(16 V)/(3\[Pi] d^2)


TauEjeTorsion[T_,d_]:=(16 T)/(\[Pi] d^3)


End[ ]
EndPackage[ ]
