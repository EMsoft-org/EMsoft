//
//  main.c
//  
//
//  Created by Prabhat KC on 10/7/15.
//
// this file implements MAP estimate of a noisy and blurry image
// using non - gaussian prior. More specifically, this program
// makes use of surrogate majorization technique to implement
// non-gaussian prior. For the same reason, this implementation
// takes shorter time than Newton-Rahpson to go through the each
// iteration.

/*!--------------------------------------------------------------------------
!
! SUBROUTINE:denoise.c
!
!> @author Saransh Singh, Carnegie Mellon University
!
!> @brief denoise a kinematical PED pattern
!
!> @param pednl namelist file for ebsd
!
!> @date 07/10/15  PKC 1.0 original
!> @date 11/23/15  SS  1.1 changed name of function, replaced argc, argv input, 
!  replaced file operation by array input/output
!--------------------------------------------------------------------------*/


#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#include <string.h>
#include "mbirHeader.h"

/*=============================================variables declaration==========================================*/
int i, j, k, l, Nx, Ny, icdStep, Nicd, NNx, NNy, Nx2pad, Ny2pad;
double maxval, minval,  p, sigma2w, sigma_w, sigma_px, alpha, sum, theta1, theta2, Presigma_px, xi, xj, v, gx, min, max;
double q, T, cost_eachite, priorInfluence, noiseInfluence;
double g[3][3]={{1.0/12.0, 1.0/6.0, 1.0/12.0}, {1.0/6.0, 0.0, 1.0/6.0}, {1.0/12.0, 1.0/6.0, 1.0/12.0}};
double h[5][5]={{1.0/81.0, 2.0/81.0, 3.0/81.0, 2.0/81.0, 1.0/81.0}, {2.0/81.0, 4.0/81.0, 6.0/81.0, 4.0/81.0, 2.0/81.0}, {3.0/81.0, 6.0/81.0, 9.0/81.0, 6.0/81.0, 3.0/81.0}, {2.0/81.0, 4.0/81.0, 6.0/81.0, 4.0/81.0, 2.0/81.0}, {1.0/81.0, 2.0/81.0, 3.0/81.0, 2.0/81.0, 1.0/81.0}};
/*=========================================================================================================*/



/*=============================================calling the internal  functions===============================*/
void printarray(double inputArray[], int Nx, int Ny);
void printarray1d(double inputArray[],int Nx, int Ny);
double findlow (int i, int j, int Nx, double startingpoint, double inputArray[]);
double findhigh(int i, int j, int Nx, double startingpoint,  double inputArray[]);
double estimate_influence(double u, int i, int j, int Nx, double g[3][3], double alpha, double p, double inputArray[]);
double equation(double theta1, double theta2, double u, double v, int i, int j, int Nx, double g[3][3], double alpha, double p, double inputArray[]);
double hxval(double h[5][5], double inputArray[], int i, int j, int Nx, int Ny);
double surrogateXestimage(double v, double sigma2w, double h[5][5], double theta2, double inputArray[], double errorArray[], double bsr[], int i, int j, int NNx, int Nx2pad, double priorInfluence, double noiseInfluence);
double cost1(double sigma2w, double h[5][5], double matrix_y[], double matrix_x[], int Nx, int Ny, int Nx2pad, int Ny2pad);
double cost2(double g[3][3], double inputArray[], double sigma_px, double p, int Nx, int Ny);
/*=====================================================================================================================*/


double* denoise (int *lx, int *ly, float *pf, float *powp, float *sigma_w, float *prinf, float *noisinf, int *icd, double* mbike)
{
        Nx=*lx; Ny=*ly;
        NNx=Nx+2, NNy=Ny+2, Nx2pad=Nx+4, Ny2pad=Ny+4, Nicd=*icd;
        p=*powp; sigma2w=(*sigma_w)*(*sigma_w);
        priorInfluence=*prinf, noiseInfluence=*noisinf;
        q=2.0, T=0.0001;
        theta2=0.0550221/sigma2w; Presigma_px=*pf;
        /*==========================allocate all the arrays to be used here===============================*/
        double *blurrpic=(double*)malloc(sizeof(double)*Ny*Nx);
        double *blurrnoisypic=(double*)malloc(sizeof(double)*Ny*Nx);
        double *img_y=(double*)malloc(sizeof(double)*Ny*Nx);
        double *img_x=(double*)malloc(sizeof(double)*Ny*Nx);
        double *img_e=(double*)malloc(sizeof(double)*Ny*Nx);
        double *img_v=(double*)malloc(sizeof(double)*Ny*Nx);
        
        
        double *pad1dblurrpic=(double*)malloc(sizeof(double)*NNy*NNx);
        double *img_v1dpad=(double*)malloc(sizeof(double)*NNy*NNx);
        
        double *counter2dpad=(double*)malloc(sizeof(double)*Ny2pad*Nx2pad);
        double *psuedoblurr=(double*)malloc(sizeof(double)*Ny2pad*Nx2pad);
        double *paded2dimg_e=(double*)malloc(sizeof(double)*Ny2pad*Nx2pad);

        double newg[9];
        double* totalcost = malloc(sizeof(double) * Nicd);
        
        /*================================================================================================*/
 // replaced file read by array input       
        
        maxval=maximum(mbike, Nx, Ny); minval=minimum(mbike, Nx, Ny);
        pad1d(mbike, pad1dblurrpic, Nx, Ny);
        sigma_px=pow(Presigma_px, p)*estimate_sigmapx(g, pad1dblurrpic, p, NNx, NNy);
        equalArray(mbike, img_y, Nx, Ny);
        equalArray(img_y, img_x, Nx, Ny);

        /*================================================================================================*/
        
        
        /*===============================ICD implemenations here=========================================*/
        pad2d(img_x, counter2dpad, Nx, Ny);
        hxpic(h, counter2dpad, psuedoblurr, Nx2pad, Ny2pad);
        decouple2dpad(psuedoblurr, blurrpic, Nx2pad, Ny2pad);/*output array is of Nx Ny*/
        as2dArray(img_y, blurrpic, img_e, -1.0, Nx, Ny);
        pad2d(img_e, paded2dimg_e, Nx, Ny);
        alpha=1.0/sigma_px;

        equalArray(img_x, img_v, Nx, Ny);
        pad1d(img_v, img_v1dpad, Nx, Ny);

        for (icdStep=0; icdStep<Nicd;  icdStep++)
        {
            for (i=1; i<(NNy-1); i++)
            for (j=1; j<(NNx-1); j++)
            {
                v=img_v1dpad[i*NNx+j];
                barbsr(img_v1dpad, newg, g, p, q, T, sigma_px, i, j, NNx);
                xi=surrogateXestimage(v, sigma2w, h, theta2, img_v1dpad, paded2dimg_e, newg, i, j, NNx, Nx2pad, priorInfluence, noiseInfluence);
                if(xi>maxval) xi=maxval;
                if (xi<minval) xi=minval;
                img_v1dpad[i*NNx+j]=xi;

                for (k=-2; k<3; k++)
                for (l=-2; l<3; l++)
                {
                    paded2dimg_e[((i+1)+k)*Nx2pad+((j+1)+l)]=paded2dimg_e[((i+1)+k)*Nx2pad+((j+1)+l)]-h[k+2][l+2]*(xi-v);
                }
            }
            decouple2dpad(paded2dimg_e, img_e, Nx2pad, Ny2pad);
            pad2d(img_e, paded2dimg_e, Nx, Ny);
            decouple1dpad(img_v1dpad, img_v, NNx, NNy);
            pad1d(img_v, img_v1dpad, Nx, Ny);
            totalcost[icdStep]=cost1(sigma2w, h, img_y, img_v, Nx, Ny, Nx2pad, Ny2pad)+cost2(g, img_v1dpad, sigma_px, p, NNx, NNy);
        }

        /*===============================================================================================*/

// replaced file write to array output

    //if(strncmp(out, "blurr_pic", 1)==0) printarray1d(img_y, Nx, Ny);
    //if(strncmp(out, "deblurr_pic", 1)==0) printarray1d(img_v, Nx, Ny);
    //if(strncmp(out, "cost_convergence", 1)==0) printarray1d(totalcost, Nicd, 1);
       
// 04/27/16 SS
// removed mbike and img_v from the list of pointers being freed.

    free(pad1dblurrpic);
    free(img_x), free(img_y), free(counter2dpad);
    free(psuedoblurr), free(blurrpic);
    free(img_e), free(paded2dimg_e);
    free(img_v1dpad);
    free(totalcost);
    return img_v;
}

double surrogateXestimage(double v, double sigma2w, double h[5][5], double theta2, double inputArray[], double errorArray[], double bsr[], int i, int j, int NNx, int Nx2pad, double priorInfluence, double noiseInfluence)
{
    theta1=0.0;
    double sumBX=0.0, sumb=0.0;
    
    for (k=-2; k<3; k++)
    for (l=-2; l<3; l++)
    {
        theta1+=-errorArray[(k+i+1)*Nx2pad+(l+j+1)]*h[k+2][l+2]; /*here +1 scaling is due to the fact that error matrix is paded twice with pad2 */
    }
    
    theta1=theta1/sigma2w;
    for (k=-1; k<2; k++)
    for (l=-1; l<2; l++)
    {
        sumb+=bsr[(k+1)*3+(l+1)];
        sumBX+=bsr[(k+1)*3+(l+1)]*inputArray[(i+k)*NNx+(j+l)];
    }
    return ((noiseInfluence*(theta2*v-theta1)+2.0*priorInfluence*sumBX)/(noiseInfluence*theta2+2.0*priorInfluence*sumb));
}


double cost1(double sigma2w, double h[5][5], double matrix_y[], double matrix_x[], int Nx, int Ny, int Nx2pad, int Ny2pad)
{
    double *counter2dpad=(double*)malloc(sizeof(double)*Ny2pad*Nx2pad);
    double *psuedohx=(double*)malloc(sizeof(double)*Ny2pad*Nx2pad);
    double *matrix_Hx=(double*)malloc(sizeof(double)*Ny*Nx);
    double *diff_y_hx=(double*)malloc(sizeof(double)*Ny*Nx);
    double *normMatrix=(double*)malloc(sizeof(double)*Ny*Nx);

    pad2d(matrix_x, counter2dpad, Nx, Ny);
    hxpic(h, counter2dpad, psuedohx, Nx2pad, Ny2pad);
    decouple2dpad(psuedohx, matrix_Hx, Nx2pad, Ny2pad);
    matrixdifference(matrix_y, matrix_Hx, diff_y_hx, Nx, Ny);
    elementwise2dmult(diff_y_hx, diff_y_hx, normMatrix, Nx, Ny);

    
    sum=0.0;
    for (i=0; i<(Ny); i++)
    for (j=0; j<(Nx); j++)
    {
        sum+=normMatrix[i*Nx+j];
    }
    free(counter2dpad), free(psuedohx), free(matrix_Hx), free(diff_y_hx), free(normMatrix);
    return (sum/(2.0*sigma2w));
}



double cost2(double g[3][3], double inputArray[], double sigma_px, double p, int Nx, int Ny)
{
    sum=0.0;
    for (i=1; i<(Ny-1); i++)
    for (j=1; j<(Nx-1); j++)
    {
        for (k=-1; k<2; k++)
        for (l=-1; l<2; l++)
        {
            sum+=g[k+1][l+1]*pow(fabs(inputArray[i*Nx+j]-inputArray[(i+k)*Nx+(j+l)]),p);
        }
    }
    
    sum=sum/(p*sigma_px);
    return sum;
}



double hxval(double h[5][5], double inputArray[], int i, int j, int Nx, int Ny)
{
    sum=0.0;
    for (k=-2; k<3; k++)
    for (l=-2; l<3; l++)
    {
        sum+=h[k+2][l+2]*inputArray[(i+k)*Nx+(j+l)];
    }
    return (sum);
}


