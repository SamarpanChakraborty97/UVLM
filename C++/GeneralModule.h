#pragma once
#ifndef GENERALMODULE_H
#define GENERALMODULE_H

struct GeneralData {
	int nab;			// number of aerodynamic bodies
	int naw;			// number of wake bodies
	int neb;			// number of elastic bodies
	int nspring;		// number of spring bodies
	int nstep;			// total number of simulation steps
	int dop;			// degree of matrix
	int flag_tec;		// flag if tecplot is to be used
	int flag_matlab;	// flag if matlab is to be used
	double dt;			// time step
	double tol;			// tolerance for convergence
	double vc;			// characteristic velocity
	double tc;			// characteristic time
	double lc;			// characteristic length
	double cl;			// lift coefficient
	double cd;			// drag coefficient
	double l;			// length
	double d;			// 
	double rho;			// density
	int* idabc;			// 
	double l_ref;		// reference length
	double u_inf_x;		// free stream x-velocity
	double u_inf_y;		// free stream y-velocity
	double x_eD;		// x-direction drag vector
	double y_eD;		// y-direction drag vector
	double x_eL;		// x-direction lift vector
	double y_eL;		// y-direction lift vector
	double cutoffb;		// body cut-off
	double cutoffw;		// wake cut-off
};

#endif  // !GENERALMODULE_H

