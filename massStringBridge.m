(* ::Package:: *)

(* ::Title:: *)
(*Mass String Bridge*)


(* ::Input:: *)
(*ClearAll["Global`*"]*)
(*$Assumptions={Subscript[y, _]>0&&Subscript[m, _]>0&&Subscript[F, _]>0&&Subscript[T, _]>0&&Subscript[L, 1]>0&&Subscript[L, 2]>0&&Subscript[L, 3]>0&&Subscript[L, 4]>0}*)


(* ::Section:: *)
(*Setup*)


(* ::Text:: *)
(*Establish variables for Sin[Subscript[\[Theta], n]] = Subscript[H, n]*)


(* ::Input:: *)
(*Subscript[H, 1]=Subscript[y, 1][t]/Sqrt[Subscript[L, 1]^2+(Subscript[y, 1][t])^2];*)
(*Subscript[H, 2]=(Subscript[y, 2][t]-Subscript[y, 1][t])/Sqrt[Subscript[L, 2]^2+(Subscript[y, 2][t]-Subscript[y, 1][t])^2];*)
(*Subscript[H, 3]=(Subscript[y, 3][t]-Subscript[y, 2][t])/Sqrt[Subscript[L, 3]^2+(Subscript[y, 3][t]-Subscript[y, 2][t])^2];*)
(*Subscript[H, 4]=-Subscript[y, 3][t]/Sqrt[Subscript[L, 4]^2+(Subscript[y, 3][t])^2];*)


(* ::Section:: *)
(*Equations of Motion*)


(* ::Text:: *)
(*Assuming x axis is horizontal and positive towards the right. The y axis is vertical and positive points up. I define position vectors for all masses:*)


(* ::Input:: *)
(*Subscript[r, 1]={Subscript[L, 1],Subscript[y, 1][t]};*)
(*Subscript[r, 2]={Subscript[r, 1][[1]]+Subscript[L, 2],Subscript[y, 2][t]};*)
(*Subscript[r, 3]={Subscript[r, 2][[1]]+Subscript[L, 3],Subscript[y, 3][t]};*)
(*Subscript[r, 4]={Subscript[r, 3][[1]]+Subscript[L, 4],0};*)


(* ::Text:: *)
(*Define kinetic energy of a system:*)


(* ::Input:: *)
(*KE=Sum[1/2 Subscript[m, k ](D[Subscript[r, k][[1]],t]^2+D[Subscript[r, k][[2]],t]^2),{k,3}]//Simplify*)


(* ::Text:: *)
(*Define potential energy of a system:*)


(* ::Input:: *)
(*PE=-(Subscript[m, 1]g Subscript[r, 1][[2]]+Subscript[m, 2]g Subscript[r, 2][[2]]+Subscript[m, 3]g Subscript[r, 3][[2]])//Expand*)


(* ::Input:: *)
(*Lg=KE-PE//Expand*)


(* ::Text:: *)
(*Define virtual work of a system:*)


(* ::Input:: *)
(*Subscript[\[Delta]W, 1] = (Subscript[F, 1][t]+T (-Subscript[H, 1]+Subscript[H, 2]))D[Subscript[r, 1][[2]],t]/.Derivative[1][Subscript[y, n_]][t]-> Subscript[\[Delta]y, n]//Expand*)
(*Subscript[\[Delta]W, 2] = (Subscript[F, 2][t]+T (-Subscript[H, 2]+Subscript[H, 3]))D[Subscript[r, 2][[2]],t]/.Derivative[1][Subscript[y, n_]][t]-> Subscript[\[Delta]y, n]//Expand*)
(*Subscript[\[Delta]W, 3] = (Subscript[F, 3][t]+T (-Subscript[H, 3]+Subscript[H, 4]))D[Subscript[r, 3][[2]],t]/.Derivative[1][Subscript[y, n_]][t]-> Subscript[\[Delta]y, n]//Expand*)


(* ::Text:: *)
(*Preallocate memory for the equations of motion:*)


(* ::Input:: *)
(*eq={Null,Null,Null};*)


(* ::Text:: *)
(*Assemble equations of motion:*)


(* ::Input:: *)
(*Do[*)
(*Print[*)
(*eq[[k]]=D[D[Lg,Derivative[1][Subscript[y, k]][t]],t]+D[Lg,Subscript[y, k][t]]==Coefficient[Subscript[\[Delta]W, k],Subscript[\[Delta]y, k]]*)
(*],{k,3}*)
(*]//Simplify*)


(* ::Section:: *)
(*Equilibrium Points*)


(* ::Text:: *)
(*Define the rules for equilibrium points and use them to simplify the equations of motion:*)


(* ::Input:: *)
(*EqRules={Derivative[1][Subscript[y, _]][t]->0,(Subscript[y, _]^\[Prime]\[Prime])[t]->0,Subscript[F, _][t]->0};*)


(* ::Input:: *)
(*EqEqs=eq/.EqRules*)


(* ::Text:: *)
(*Set refined rules for equilibrium points to solve for when there are small variations in Subscript[y, n][t]. If you do not make this assumption, it will not be able to quickly solve for the equilibrium points. *)


(* ::Input:: *)
(*EqRulesSmall = EqEqs/.(-Subscript[y, 1][t]+Subscript[y, 2][t])->0 /. (-Subscript[y, 2][t]+Subscript[y, 3][t])->0 /.Subscript[y, 1][t]^2 -> 0 /.Subscript[y, 3][t]^2->0*)


(* ::Text:: *)
(*Solve the simplified equations of motion to determine the equilibrium points: *)


(* ::Input:: *)
(*EqSols=Solve[EqRulesSmall,{Subscript[y, 1][t],Subscript[y, 2][t],Subscript[y, 3][t]}]/.{C[_]->0} //Simplify*)


(* ::Section:: *)
(*Linearized Equations of Motion About Equilibrium Points*)


(* ::Text:: *)
(*Prepare for linearization about a general equilibrium point (this is a trick to do multivariate Taylor expansion):*)


(* ::Input:: *)
(*subs={Subscript[y, p_][t]->Subscript[y, p][t]q,Derivative[1][Subscript[y, p_]][t]->Derivative[1][Subscript[y, p]][t]q,(Subscript[y, p_]^\[Prime]\[Prime])[t]->(Subscript[y, p]^\[Prime]\[Prime])[t]q};*)


(* ::Input:: *)
(*taylorEqs=eq/.subs;*)


(* ::Text:: *)
(*Using the trick from above, this returns the Linearize equations about some equilibrium point (Subscript[ye, 1], Subscript[ye, 2], Subscript[ye, 3]) are:*)


(* ::Input:: *)
(*linearizedEqs=Normal[*)
(*Series[taylorEqs,{q,0,1}]*)
(*]/.{q->1}*)


(* ::Section:: *)
(*Derive Eigenvalues*)


(* ::Text:: *)
(*Assign the parameters that are given in the problem: Subscript[m, i]=m and Subscript[L, i]=L*)


(* ::Text:: *)
(*Set new variable names for the equations of motion composed from the newly assigned parameters: *)


(* ::Input:: *)
(*EqualMassEq=linearizedEqs/.Subscript[m, 1]-> m/.Subscript[m, 2]-> m/.Subscript[m, 3]-> m/.Subscript[L, 1]-> L/.Subscript[L, 2]-> L/.Subscript[L, 3]-> L/.Subscript[L, 4]-> L*)
(**)


(* ::Text:: *)
(*The equations of motion for small oscillations about the stable equilibrium are:*)


(* ::Input:: *)
(*Mm=Normal[CoefficientArrays[EqualMassEq,Table[Subscript[y, k]''[t],{k,3}]]][[2]]/(m); Mm//MatrixForm*)
(*Km=Normal[CoefficientArrays[EqualMassEq,Table[Subscript[y, k][t],{k,3}]]][[2]]/(T/L); Km//MatrixForm*)
(*Fm=EqualMassEq[[All,2]];Fm//MatrixForm*)


(* ::Text:: *)
(*Below are the eigenvectors of the system:*)


(* ::Input:: *)
(*EqualMassES=Eigensystem[{Km,Mm}//N]*)


(* ::Section:: *)
(*Modal Analysis*)


(* ::Text:: *)
(*Normal modes:*)


(* ::Input:: *)
(*EqualMassNM=EqualMassES[[2]]*)


(* ::Text:: *)
(*Natural frequencies: *)


(* ::Input:: *)
(*EqualMass\[Omega]=Sqrt[EqualMassES[[1]] T/(L m)]//MatrixForm*)


(* ::Text:: *)
(*Plot modes:*)


(* ::Input:: *)
(*plt = Show[ListLinePlot[Table[Thread[{{0,1,2,3, 4},Insert[EqualMassNM,0,{{1,1},{1,4},{2,1},{2,4},{3,1},{3,4}}][[k]]}],{k,3}],PlotStyle->{Thickness[0.01]},GridLines->Automatic,PlotRange->{{0,4.1},{-1,1}}],ListPlot[Table[Thread[{{1,2,3},EqualMassNM[[k]]}],{k,3}],PlotStyle->PointSize[0.03],GridLines->Automatic]]*)


(* ::Text:: *)
(*These modes, listed above as "EqualMassNM", represent the modes of the system. Due to the constraints of the problem, the endpoint of the rightmost mass should also be constrained to the wall (theoretically at point (0,4)). The shape of one mode is when two masses are downwards while one mass is upwards. Another mode is when the central mass is nearly located on the horizontal, but the other two are on opposite sides of the horizontal. The third is when all three masses are upwards, and thus each mode is unique. These modes are used to describe the free vibration of the system. If these modes were given physical meaning, then they would show the normalized approximate displacement when each frequency is excited individually. The first mode has the highest frequency and is clearly more oscillatory than the second or third modes. The third mode has the lowest frequency and is clearly the least oscillatory of the three modes. *)


(* ::Section:: *)
(*Orthogonality of Normal Modes*)


(* ::Text:: *)
(*Verify that the natural modes from problem 7.19 are orthogonal. This is done via equation 7.88 from the book, but first we need to find which mode is Subscript[U, s] by checking that Subscript[U, s] K = 0.*)


(* ::Input:: *)
(*Subscript[check, 1]= EqualMassNM[[1]].Km*)
(*Subscript[check, 2]= EqualMassNM[[2]].Km*)
(*Subscript[check, 3]= EqualMassNM[[3]].Km*)


(* ::Text:: *)
(*Once I determine the correct Subscript[U, s], I can use the equation 7.88 from the book to check whether the modes are orthogonal:*)


(* ::Input:: *)
(*orthogonal31=EqualMassNM[[3]].Mm.EqualMassNM[[1]]*)


(* ::Input:: *)
(*orthogonal32=EqualMassNM[[3]].Mm.EqualMassNM[[2]]*)


(* ::Text:: *)
(*Both of these check values should be equal to zero to verify that they are orthogonal. Although they are not exactly zero, they are extremely close. *)


(* ::Section:: *)
(*Normalization of Normal Modes *)


(* ::Text:: *)
(*Normalize the natural modes from problem 7.19 via equation 7.92, which is U^T M U = \[CapitalOmega]*)


(* ::Input:: *)
(*normalizedNM = EqualMassNM.Km.EqualMassNM//MatrixForm*)


(* ::Text:: *)
(*This matrix should be a diagonal matrix of the natural frequencies squared. *)


(* ::Section:: *)
(*Natural Frequencies*)


(* ::Text:: *)
(*Set new variable names for the equations of motion composed from the newly assigned parameters: *)


(* ::Input:: *)
(*BiasedMassEq=linearizedEqs/.Subscript[m, 1]-> m/.Subscript[m, 2]-> m/.Subscript[m, 3]-> 2 m/.Subscript[L, 1]-> L/.Subscript[L, 2]-> L/.Subscript[L, 3]-> L/.Subscript[L, 4]-> L*)


(* ::Text:: *)
(*Identify the corresponding system matrices:*)


(* ::Input:: *)
(*Mm=Normal[CoefficientArrays[BiasedMassEq,Table[Subscript[y, k]''[t],{k,3}]]][[2]]/m; Mm//MatrixForm*)
(*Km=Normal[CoefficientArrays[BiasedMassEq,Table[Subscript[y, k][t],{k,3}]]][[2]]/(T/L); Km//MatrixForm*)
(*Fm=BiasedMassEq[[All,2]]; Fm//MatrixForm*)


(* ::Text:: *)
(*Find natural frequencies and the modes:*)


(* ::Input:: *)
(*BiasedMassES=Eigensystem[{Km,Mm}//N]*)


(* ::Text:: *)
(*Normal modes:*)


(* ::Input:: *)
(*BiasedMassNM=BiasedMassES[[2]]*)


(* ::Text:: *)
(*Natural frequencies:*)


(* ::Input:: *)
(*BiasedMass\[Omega]=Sqrt[BiasedMassES[[1]] T/(L m)]//MatrixForm*)


(* ::Input:: *)
(*plt = Show[ListLinePlot[Table[Thread[{{0,1,2,3, 4},Insert[BiasedMassNM,0,{{1,1},{1,4},{2,1},{2,4},{3,1},{3,4}}][[k]]}],{k,3}],PlotStyle->{Thickness[0.01]},GridLines->Automatic,PlotRange->{{0,4.1},{-1,1}}],ListPlot[Table[Thread[{{1,2,3},BiasedMassNM[[k]]}],{k,3}],PlotStyle->PointSize[0.03],GridLines->Automatic]]*)


(* ::Text:: *)
(*It is clear that the mode shapes of the system with a mass bias are very similar to the mode shapes of the system with equal masses, but there are also some distinct differences. The third mode corresponds to the lowest frequency in the system (indicated in green on the figure) is entirely the opposite phase between the two systems. The other two modes clearly look very similar to the shapes from the system with equal masses, but there is a strong bias toward the third (heavier) mass. The oscillations of the system here lose some of their symmetric behavior as well. *)
