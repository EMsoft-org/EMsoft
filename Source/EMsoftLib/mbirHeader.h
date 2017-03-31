//
//  Header.h
//  
//
//  Created by Prabhat KC on 6/15/15.
//
//

#ifndef _Header_h
#define _Header_h

void as2dArray(double inputArray1[], double inputArray2[], double outputArray[], double AS, int Nx, int Ny);
void barbsr(double inputArray[], double outputArray[], double g[3][3], double p, double q, double T, double sigma_px, int i, int j, int N);
void decouple1dpad(double inputArray[], double outputArray[], int NNx, int NNy);
void decouple2dpad(double inputArray[], double outputArray[], int Nx2pad, int Ny2pad);
void elementwise2dmult(double inputArray1[], double inputArray2 [], double outputArray[], int Nx, int Ny);
void equalArray(double inputArray[], double outputArray[], int Nx, int Ny);
double estimate_sigmapx(double g[3][3], double inputArray[], double p, int Nx, int Ny);
void hxpic(double h[5][5], double inputArray[], double outputArray[], int Nx, int Ny);
void matrixdifference(double inputArray1[], double inputArray2[], double outputArray[], int Nx, int Ny);
double maximum(double inputArray[], int Nx, int Ny);
double minimum(double inputArray[], int Nx, int Ny);
double *multiplysinglefactor(double factor, double inputArray[], int Nx, int Ny);
void pad1d (double inputArray[], double outputArray[], int Nx, int Ny);
void pad2d (double inputArray[], double outputArray[], int Nx, int Ny);
double *sumgx(double g[3][3], double inputArray[], int Nx, int Ny);

#endif
