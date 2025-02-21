#pragma once
#ifndef AERODYNAMICMODULE_H
#define AERODYNAMICMODULE_H

struct AeroBody {
	int np;				//Number of panels
	int nnp;			//Number of nodal points
	int ncp;			//Number of control points
	int nvp;			//Number of vortex points

	double cl;			//Lift coefficient

	///// Locations of the different points in the aerofoil //////

	double* x_np_b;		//Nodal points array of x : before
	double* y_np_b;		//Nodal points array of y : before
	double* z_np_b;		//Nodal points array of z : before
	double* x_cp_b;		//Control points array of x : before
	double* y_cp_b;		//Control points array of y : before
	double* z_cp_b;		//Control points array of z : before
	double* x_vp_b;		//Vortex points array of x : before
	double* y_vp_b;		//Vortex points array of y : before
	double* z_vp_b;		//Vortex points array of z : before

	double* x_np_a;		//Nodal points array of x : after
	double* y_np_a;		//Nodal points array of y : after
	double* z_np_a;		//Nodal points array of z : after
	double* x_cp_a;		//Control points array of x : after
	double* y_cp_a;		//Control points array of y : after
	double* z_cp_a;		//Control points array of z : after
	double* x_vp_a;		//Vortex points array of x : after
	double* y_vp_a;		//Vortex points array of y : after
	double* z_vp_a;		//Vortex points array of z : after

	///// Velocities of the control point related variables on the aerofoil /////

	double* x_vel_cp_w;	//x-Velocity array of control points at the wake surface
	double* y_vel_cp_w;	//y-Velocity array of control points at the wake surface
	double* z_vel_cp_w;	//z-Velocity array of control points at the wake surface
	double* x_vel_cp_s;	//x-Velocity array of control points at the body surface
	double* y_vel_cp_s;	//y-Velocity array of control points at the body surface
	double* z_vel_cp_s;	//z-Velocity array of control points at the body surface

	//// Normal force

	double* x_FN;		//Normal force x-array
	double* y_FN;		//Normal force y-array
	double* z_FN;		//Normal force z-array

	//// Vectors on the aerofoil surface

	double* x_nV_b;		//normal vector x array : before
	double* y_nV_b;		//normal vector y array : before
	double* z_nV_b;		//normal vector z array : before
	double* x_nV_a;		//normal vector x array : after
	double* y_nV_a;		//normal vector y array : after
	double* z_nV_a;		//normal vector z array : after
	
	double* x_tV_b;		//tangential vector x array : before
	double* y_tV_b;		//tangential vector y array : before
	double* z_tV_b;		//tangential vector z array : before
	double* x_tV_a;		//tangential vector x array : after
	double* y_tV_a;		//tangential vector y array : after
	double* z_tV_a;		//tangential vector z array : after

	//// circulation and pressure variables

	double del_CP;		// pressure coefficient 
	double* Ck;			// array of circulation strengths
	double* BG;			// boundary condition coefficients
	double* BDG;		// RHS array of boundary conditions
	double SUMBDG;		// sum of RHS array

	//// lattice mapping

	double* LM_1;		// indices array of the starting control point
	double* LM_2;		// indices array of the end control point
};

struct Wake {
	int nvp;			// number of vortex points in the wake
	int limnvp;			// maximum possible vortex points in the wake
	double* WG;			// wake strength array
	double* x_N;		// node x-position array
	double* y_N;		// node y-position array
	double* z_N;		// node z-position array
	double* x_AUX;		// auxiliary x-position array
	double* y_AUX;		// auxiliary y-position array
	double* z_AUX;		// auxiliary z-position array
};

#endif // !AERODYNAMICMODULE_H

