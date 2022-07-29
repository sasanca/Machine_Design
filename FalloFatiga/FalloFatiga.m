(* ::Package:: *)

(* ::Text:: *)
(*(* :Title: FalloFatiga.m -- a package template *)*)
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
(*(* :Package Version: 0.0.3 *)*)
(**)
(*(* :Mathematica Version: 10.0 *)*)
(**)
(*(* :History:*)
(*   0.0.1 Versi\[OAcute]n inicial*)
(*   0.0.2 Primera distribuci\[OAcute]n. Numerosos bugs. No funcionan las llamadas a funci\[OAcute]n.*)
(*   0.0.3 Segunda distribuci\[OAcute]n. Se arreglan los bugs m\[AAcute]s importantes. Se incluye coef. de Goodman*)
(**)*)
(**)
(*(* :Keywords: template, fallo, fatiga, package *)*)
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


(* set up the package context, including public imports *)

BeginPackage["FalloFatiga`"]
(* Exported symbols added here with SymbolName::usage *) 


Facabado::usage="Facabado[acabado,Sut] calcula el factor de acabado en funci\[OAcute]n del grado de acabado N4-N11. Para definir el grado de acabado se introduce solo el valor num\[EAcute]rico";


Ftamano::usage="Ftamano[tipodecarga,d] calcula el factor de tama\[NTilde]o en funci\[OAcute]n del tipo de carga: traccion, compresion, flexion o torsion y el diametro. El tipo de carga se define sin acentos y entre comillas.";


Fcarga::usage="Fcarga[tipodecarga,Sut] calcula el factor de carga en funci\[OAcute]n del tipo de carga: traccion, compresion, flexion o torsion. El tipo de carga se define sin acentos y entre comillas.";


Ftemperatura::usage="Ftemperatura[T] calcula el factor de temperatura para los aceros. La temperatura se introduce mediante su valor num\[EAcute]rico, en grados Celsius. No se debe utilizar para otros materiales.";


Fconfiabilidad::usage="Fconfiabilidad[p] calcula el factor de confiabilidad para los materiales met\[AAcute]licos de uso habitual. La confiabilidad se introduce mediante su valor num\[EAcute]rico de forma porcentual. No se debe utilizar para otros materiales.";


FSeK::usage="FSeK[acabado,Sut,tipodecarga,diametro,T,confiabilidad,Kf] calcula el l\[IAcute]mite de resistencia a la fatiga corregido de los aceros. No emplear para otros materiales.";


Fraiza::usage="Fraiza[material,Sut] calcula la constante de neuber para los aceros y aluminios. El material se introduce como: acero, aluminio o duraluminio (entre comillas). No emplear para otros materiales.";


Fq::usage="Fq[material,Sut,r] calcula la sensibilidad a la entalla para los aceros y aluminios. El material se introduce como: acero, aluminio o duraluminio (entre comillas). No emplear para otros materiales.";


FKf::usage="FKf[material,Kt,Sut,r] calcula el concentrador de esfuerfos corregido a fatiga para los aceros y aluminios. El material se introduce como: acero, aluminio o duraluminio (entre comillas). No emplear para otros materiales.";


nGoodman::usage="nGoodman[\[Sigma]m,\[Sigma]a,Sut,Se] calcula el coeficiente de seguridad de Goodman";


Sigmaa0::usage="Sigmaa0[\[Sigma]m,\[Sigma]a,Sut] calcula la tensi\[OAcute]n alternate equivalente a un estado tensional (\[Sigma]a,\[Sigma]m) seg\[UAcute]n la recta de Goodman";


FNW::usage="FNW[\[Sigma]a,N1,N2,S1,S2] calcula la duraci\[OAcute]n para una tensi\[OAcute]n alternante \[Sigma]a dada.";


FSW::usage="FSW[Na,N1,N2,S1,S2] calcula la m\[AAcute]xima tensi\[OAcute]n alternante para una duraci\[OAcute]n Na dada.";


FSeKS::usage="FSeKS[Nrestantes,N1,N2,S1,\[Sigma]a] calcula el nuevo l\[IAcute]mite de resistencia a la fatiga si quedan Nrestantes ciclos de duraci\[OAcute]n trabajando a \[Sigma]a.";


Begin["`Private`"]
(* Implementation of the package *)



(* ::Section:: *)
(*Formulario de Fatiga*)


(* ::Subsection:: *)
(*C\[AAcute]lculo del l\[IAcute]mite de resistencia a la fatiga*)


(* ::Subsubsection:: *)
(*Factor de acabado*)


Facabado[acabado_,Sut_]:=If[acabado<=4,1,If[acabado<=6,1.58 Sut^-0.085,If[acabado<=9, 4.51 Sut^-0.265, If[acabado<=11, 57.7 Sut^-0.718, 272 Sut^-0.995]]]]


(* ::Subsubsection:: *)
(*Factor de tama\[NTilde]o*)


Ftamano[tipodecarga_,d_]:= Which["traccion"==tipodecarga ||"compresion"==tipodecarga,1,"flexion" ==tipodecarga||"torsion"==tipodecarga, Which[d<=8,1,d>8&&d<=250,1.189 d ^-0.097,d>250,0]]
(*tipodecarga "traccion", "compresion", "flexion", "torsion". Se deben introducir sin tildes y entre comillas*)


(* ::Subsubsection:: *)
(*Factor de carga*)


Fcarga[tipodecarga_,Sut_]:=Which["flexion" ==tipodecarga||"torsion"==tipodecarga ||"compresion"==tipodecarga,1,"traccion"==tipodecarga , 1.43 Sut^-0.078](*tipodecarga "traccion", "compresion", "flexion", "torsion". Se deben introducir sin tildes y entre comillas*)


(* ::Subsubsection:: *)
(*Factor de temperatura*)


Ftemperatura[T_]:=If[T<0,0,If[T<=450,1,If[T<=550, 1-0.0058(T-450),0]]]


(* ::Subsubsection:: *)
(*Factor de confiabilidad*)


Fconfiabilidad[P_]:=If[P<=50,1,If[P<=90,.897,If[P<=95,0.87,If[P<= 99,.814,If[P<=99.9,0.753,If[P<=99.99,0.702,If[P<=99.999,0.659, 0.62]]]]]]]


(* ::Subsubsection:: *)
(*L\[IAcute]mite de resistencia a la fatiga corregido para aceros*)


FSeK[acabado_,Sut_,tipodecarga_,diametro_,T_,confiabilidad_,Kf_]:=0.504Sut Facabado[acabado,Sut] Ftamano[tipodecarga
,diametro]  Fcarga[tipodecarga
,Sut] Ftemperatura[T]Fconfiabilidad[confiabilidad]/Kf


(* ::Subsection:: *)
(*Concentraci\[OAcute]n de esfuerzos a fatiga*)


(* ::Subsubsection:: *)
(*C\[AAcute]lculo de la constante de Neuber*)


Fraiza[material_,Sut_]:=Which["acero" ==material,-0.32865+34.5452 Sut^-0.60977,"aluminio" ==material,-0.29486+77.4708Sut^-0.78374,"duraluminio" ==material,0.0634+101.97946Sut^-0.81409](*material "acero", "aluminio", "duraluminio". Se deben introducir sin tildes y entre comillas*)


(* ::Subsubsection:: *)
(*C\[AAcute]lculo de la sensibilidad a la entalla*)


Fq[material_,Sut_,r_]:=1/(1+Fraiza[material,Sut]/(\[Sqrt]r))


(* ::Subsubsection:: *)
(*C\[AAcute]lculo del concentrador de tensiones a la fatiga*)


FKf[material_,Kt_,Sut_,r_]:=1+Fq[material,Sut,r](Kt-1)


(* ::Subsection:: *)
(*Coeficientes de seguridad a fatiga*)


(* ::Subsubsection:: *)
(*Coeficiente de seguridad de Goodman*)


nGoodman[\[Sigma]m_,\[Sigma]a_,Sut_,Se_]:= 1/((UnitStep[\[Sigma]m]\[Sigma]m)/Sut + \[Sigma]a/Se)


(* ::Subsection:: *)
(*Tensi\[OAcute]n alternate equivalente*)


Sigmaa0[\[Sigma]m_,\[Sigma]a_,Sut_]:= \[Sigma]a/(1 - \[Sigma]m/Sut)


(* ::Subsection:: *)
(*Duracion vs tension*)


(* ::Subsubsection:: *)
(*C\[AAcute]lculo de la duraci\[OAcute]n para una tensi\[OAcute]n alternante \[Sigma]a dada*)


FNW[\[Sigma]a_,N1_,N2_,S1_,S2_]:=N1 (S1/\[Sigma]a)^(Log10[N2/N1]/Log10[S1/S2])


(* ::Subsubsection:: *)
(*C\[AAcute]lculo de la tensi\[OAcute]n alternante m\[AAcute]xima para un n\[UAcute]mero de ciclos Na dado*)


FSW[Na_,N1_,N2_,S1_,S2_]:=S1 (S1/S2)^(Log10[N1/Na]/Log10[N2/N1])


(* ::Subsection:: *)
(*Da\[NTilde]o acumulativo por fatiga*)


(* ::Subsubsection:: *)
(*L\[IAcute]mite de resistencia a fatiga tras el da\[NTilde]ado por sobrecarga \[Sigma]a, para Nrestantes ciclos*)


FSeKS[Nrestantes_,N1_,N2_,S1_,\[Sigma]a_]:=S1 (S1/\[Sigma]a)^(Log10[N1/N2]/Log10[Nrestantes/N1])


End[]

EndPackage[]

