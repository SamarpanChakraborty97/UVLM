#pragma once
#ifndef AUXVARIABLE_H
#define AUXVARIABLE_H

struct AuxVariable {
	double* xj;			// auxiliary coefficents 0 for numerical simulation
	double* xjm1;		// auxiliary coefficents 1 for numerical solution
	double* xjm2;		// auxiliary coefficents 2 for numerical solution
	double* xjm3;		// auxiliary coefficents 3 for numerical solution
	double* xjm4;		// auxiliary coefficents 4 for numerical solution

	double* dxjm1;		// auxiliary derivative coefficents 1 for numerical solution
	double* dxjm2;		// auxiliary derivative coefficents 2 for numerical solution
	double* dxjm3;		// auxiliary derivative coefficents 3 for numerical solution
	double* dxjm4;		// auxiliary derivative coefficents 4 for numerical solution

	double te;
};

#endif   //! AUXVARIABLE_H



