/*! ###################################################################
! Copyright (c) 2013-2014, Marc De Graef/Carnegie Mellon University
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without modification, are 
! permitted provided that the following conditions are met:
!
!     - Redistributions of source code must retain the above copyright notice, this list 
!        of conditions and the following disclaimer.
!     - Redistributions in binary form must reproduce the above copyright notice, this 
!        list of conditions and the following disclaimer in the documentation and/or 
!        other materials provided with the distribution.
!     - Neither the names of Marc De Graef, Carnegie Mellon University nor the names 
!        of its contributors may be used to endorse or promote products derived from 
!        this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
! ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE 
! LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER 
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE 
! USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! ###################################################################*/

/*!--------------------------------------------------------------------------
! EMsoft:mbir.c
!--------------------------------------------------------------------------
!
! MODULE: mbir
!
!> @author Prabhat KC, Carnegie Mellon University
!
!> @brief everything to do with denoising of ped patterns
!
!> @date 11/20/15 SS 1.0 organized all auxiliary function in one file
!> @date 05/16/16 SS 2.0 fixed memory leak
!---------------------------------------------------------------------------*/


//
//  as2dArray.c
//  
//
//  Created by Prabhat KC on 7/10/15.
//
//

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

void as2dArray(double inputArray1[], double inputArray2[], double outputArray[], double AS, int Nx, int Ny)
{
    int i, j;
    if (outputArray!=NULL)
    {
        for (i=0; i<Ny; i++)
        for (j=0; j<Nx; j++)
        {
            outputArray[i*Nx+j]=inputArray1[i*Nx+j]+AS*inputArray2[i*Nx+j];
        }
    }
}
//
//  barbsr.c
//  
//
//  Created by Prabhat KC on 7/22/15.
//
//  the barbsr estimate comes form C. Bouman's MBIR book's table 7.2 expression for QGGMRF


void barbsr(double inputArray[], double outputArray[], double g[3][3], double p, double q, double T, double sigma_px, int i, int j, int N)
{
    int k, l, index=0;
    
    double absdelta, sigmax, firstterm, secondterm, thirdterm, fourththerm;
    sigmax=pow(sigma_px, (1.0/p));
    for (k=-1; k<2; k++)
    for (l=-1; l<2; l++)
    {
        outputArray[index]=0.0;
        if (index!=4)
        {
            absdelta=fabs(inputArray[i*N+j]-inputArray[(i+k)*N+(j+l)]);
            if (absdelta==0.0)
            {
                outputArray[index]=g[k+1][l+1]/(p*sigma_px);
            }
            else
            {
                firstterm=g[k+1][l+1]*pow(absdelta, p-2.0)/(2.0*sigma_px);
                secondterm=pow(fabs(absdelta/(T*sigmax)), (q-p));
                thirdterm=(q/p)+secondterm;
                fourththerm=pow((1.0+secondterm), 2.0);
                outputArray[index]=firstterm*secondterm*thirdterm/fourththerm;
            }
        }
        index++;
    }
}

//
//  decouplepad.c
//  
//
//  Created by Prabhat KC on 7/6/15.
//
//

void decouple1dpad(double inputArray[], double outputArray[], int NNx, int NNy)
{
    int i, j, Nx, Ny;
    Nx=NNx-2; Ny=NNy-2;
    
    for (i=1; i<(NNy-1); i++)
    for (j=1; j<(NNx-1); j++)
    {
        outputArray[(i-1)*Nx+(j-1)]=inputArray[i*NNx+j];
    }
}
//
//  decouple2d.c
//  
//
//  Created by Prabhat KC on 8/6/15.
//
//


void decouple2dpad(double inputArray[], double outputArray[], int Nx2pad, int Ny2pad)
{
    int i, j, Nx, Ny;
    Nx=Nx2pad-4; Ny=Ny2pad-4;
    
    for (i=2; i<(Ny2pad-2); i++)
    for (j=2; j<(Nx2pad-2); j++)
    {
        outputArray[(i-2)*Nx+(j-2)]=inputArray[i*Nx2pad+j];
    }
}

//
//  elementwise2dmult.c
//  
//
//  Created by Prabhat KC on 6/5/15.
//
//


void elementwise2dmult(double inputArray1[], double inputArray2 [], double outputArray[], int Nx, int Ny)
{
    int i, j;
    if (outputArray!=NULL)
    {
        for (i=0; i<Ny; i++)
        for (j=0; j<Nx; j++)
        {
            outputArray[i*Nx+j]=inputArray1[i*Nx+j]*inputArray2[i*Nx+j];
        }
    }
}

//
//  equalArray.c
//  
//
//  Created by Prabhat KC on 6/1/15.
//
//


void equalArray(double inputArray[], double outputArray[], int Nx, int Ny)
{
    int i, j;
    if (outputArray!=NULL)
    {
        for (i=0; i< Ny; i++)
        for (j=0; j< Nx; j++)
        {
            outputArray[i*Nx+j]=inputArray[i*Nx+j];
        }
    }
}

//
//  estimate_sigmapx.c
//  
//
//  Created by Prabhat KC on 6/15/15.
//
//

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

double estimate_sigmapx(double g[3][3], double inputArray[], double p, int Nx, int Ny)
{
    int i, j, k, l;
    double sigma_px, sum=0.0;
    
    for (i=1; i<(Ny-1); i++)
    for (j=1; j<(Nx-1); j++)
    {
        for (k=-1; k<2; k++)
        for (l=-1; l<2; l++)
        {
            sum+=g[k+1][l+1]*pow(fabs(inputArray[(i+k)*Nx+(j+l)]-inputArray[i*Nx+j]),p);
        }
    }
    
    sigma_px=sum/(2.0*(Nx-2.0)*(Ny-2.0));
    return sigma_px;
}

//
//  hxpic.c
//  
//
//  Created by Prabhat KC on 6/16/15.
//
//


void hxpic(double h[5][5], double inputArray[], double outputArray[], int Nx, int Ny)
{
    int i, j, k, l;
    double sum;
    
    for (i=2; i<(Ny-2); i++)
    for (j=2; j<(Nx-2); j++)
    {
        sum=0.0;
        for (k=-2; k<3; k++)
        for (l=-2; l<3; l++)
        {
            sum+=h[k+2][l+2]*inputArray[(i+k)*Nx+(j+l)];
        }
        outputArray[i*Nx+j]=sum;
    }
}

//
//  matrixdifference.c
//  
//
//  Created by Prabhat KC on 6/16/15.
//
//


void matrixdifference(double inputArray1[], double inputArray2[], double outputArray[], int Nx, int Ny)
{
    int i, j;
    if (outputArray!=NULL)
    {
        for (i=0; i<Ny; i++)
        for (j=0; j<Nx; j++)
        {
            outputArray[i*Nx+j]=inputArray1[i*Nx+j]-inputArray2[i*Nx+j];
        }
    }
}

//
//  maximum.c
//  
//
//  Created by Prabhat KC on 5/31/15.
//
//


double maximum(double inputArray[], int Nx, int Ny)
{
    
    int i, j;
    double max;
    
    {
        max=inputArray[0];
        for (i=0; i<Ny; i++)
        for (j=0; j<Nx; j++)
        {
            if (inputArray[i*Nx+j]>max) max=inputArray[i*Nx+j];
            else max=max;
                
        }
    }
    return max;
}

//
//  minimum.c
//  
//
//  Created by Prabhat KC on 5/30/15.
//
//


double minimum(double inputArray[], int Nx, int Ny)
{
    
    int i, j;
    double min;
    
    {
        min=inputArray[0];
        for (i=0; i<Ny; i++)
        for (j=0; j<Nx; j++)
        {
            if (inputArray[i*Nx+j]<min) min=inputArray[i*Nx+j];
            else min=min;
            
        }
    }
    return min;
}

//
//  multiplysinglefactor.c
//  
//
//  Created by Prabhat KC on 6/17/15.
//
//


double *multiplysinglefactor(double factor, double inputArray[], int Nx, int Ny)
{
    int i, j;
    double *outputArray=(double*)malloc(sizeof(double)*Nx*Ny);
    if (outputArray!=NULL)
    {
        for (i=0; i<Ny; i++)
        for (j=0; j<Nx; j++)
        {
            outputArray[i*Nx+j]=factor*inputArray[i*Nx+j];
        }
    }
    return (outputArray);
    free(outputArray);
}

//
//  pad1d.c
//  
//
//  Created by Prabhat KC on 7/1/15.
//
//


void pad1d (double inputArray[], double outputArray[], int Nx, int Ny)
{
    int i, j, NNx=Nx+2, NNy=Ny+2;
//    double *outputArray=(double*)malloc(sizeof(double)*NNx*NNy);
    
    for (i=0; i<NNy; i++) {
    for (j=0; j<NNx; j++) {
            if (i==0 && j==0) outputArray[i*NNx+j]=inputArray[(Ny-1)*Nx+(Nx-1)];
            else if (i==0 && j==(NNx-1)) outputArray[i*NNx+j]=inputArray[(Ny-1)*Nx+0];
            else if (i==(NNy-1) && j==0) outputArray[i*NNx+j]=inputArray[0*Nx+(Nx-1)];
            else if (i==(NNy-1) && j==(NNx-1)) outputArray[i*NNx+j]=inputArray[0*Nx+0];
            else if (i==0) outputArray[i*NNx+j]=inputArray[(Ny-1)*Nx+(j-1)];
            else if (i==(NNy-1)) outputArray[i*NNx+j]=inputArray[0*Nx+(j-1)];
            else if (j==0) outputArray[i*NNx+j]=inputArray[(i-1)*Nx+(Nx-1)];
            else if (j==(NNx-1)) outputArray[i*NNx+j]=inputArray[(i-1)*Nx+0];
            else /*if (i>=1 && i<(NNx-1) && j>=1 && j<= (NNy-1))*/ outputArray[i*NNx+j]=inputArray[(i-1)*Nx+(j-1)];
        }
    }
}

//
//  pad2d.c
//  
//
//  Created by Prabhat KC on 8/6/15.
//
//


void pad2d (double inputArray[], double outputArray[], int Nx, int Ny)
{
    int i, j, Nx2pad=Nx+4, Ny2pad=Ny+4;
    for (i=0; i<Ny2pad; i++) {
    for (j=0; j<Nx2pad; j++) {
        
        if (i==0 && j==0)      outputArray[i*Nx2pad+j]=inputArray[(Ny-1)*Nx+(Nx-1)];
        else if (i==0 && j==1) outputArray[i*Nx2pad+j]=inputArray[(Ny-2)*Nx+(Nx-1)];
        else if (i==1 && j==0) outputArray[i*Nx2pad+j]=inputArray[(Ny-1)*Nx+(Nx-2)];
        else if (i==1 && j==1) outputArray[i*Nx2pad+j]=inputArray[(Ny-2)*Nx+(Nx-2)];
        
        else if (i==0 && j==(Nx2pad-1)) outputArray[i*Nx2pad+j]=inputArray[(Ny-1)*Nx+0];
        else if (i==0 && j==(Nx2pad-2)) outputArray[i*Nx2pad+j]=inputArray[(Ny-2)*Nx+0];
        else if (i==1 && j==(Nx2pad-2)) outputArray[i*Nx2pad+j]=inputArray[(Ny-2)*Nx+1];
        else if (i==1 && j==(Nx2pad-1)) outputArray[i*Nx2pad+j]=inputArray[(Ny-1)*Nx+1];

        else if (i==(Ny2pad-1) && j==0) outputArray[i*Nx2pad+j]=inputArray[0*Nx+(Nx-1)];
        else if (i==(Ny2pad-1) && j==1) outputArray[i*Nx2pad+j]=inputArray[1*Nx+(Nx-1)];
        else if (i==(Ny2pad-2) && j==0) outputArray[i*Nx2pad+j]=inputArray[0*Nx+(Nx-2)];
        else if (i==(Ny2pad-2) && j==1) outputArray[i*Nx2pad+j]=inputArray[1*Nx+(Nx-2)];
        
        else if (i==(Ny2pad-1) && j==(Nx2pad-1)) outputArray[i*Nx2pad+j]=inputArray[0*Nx+0];
        else if (i==(Ny2pad-1) && j==(Nx2pad-2)) outputArray[i*Nx2pad+j]=inputArray[1*Nx+0];
        else if (i==(Ny2pad-2) && j==(Nx2pad-1)) outputArray[i*Nx2pad+j]=inputArray[0*Nx+1];
        else if (i==(Ny2pad-2) && j==(Nx2pad-2)) outputArray[i*Nx2pad+j]=inputArray[1*Nx+1];

        else if (i==0) outputArray[i*Nx2pad+j]=inputArray[(Ny-1)*Nx+(j-2)];
        else if (i==1) outputArray[i*Nx2pad+j]=inputArray[(Ny-2)*Nx+(j-2)];

        else if (i==(Ny2pad-1)) outputArray[i*Nx2pad+j]=inputArray[0*Nx+(j-2)];
        else if (i==(Ny2pad-2)) outputArray[i*Nx2pad+j]=inputArray[1*Nx+(j-2)];
        
        else if (j==0) outputArray[i*Nx2pad+j]=inputArray[(i-2)*Nx+(Nx-1)];
        else if (j==1) outputArray[i*Nx2pad+j]=inputArray[(i-2)*Nx+(Nx-2)];

        else if (j==(Nx2pad-1)) outputArray[i*Nx2pad+j]=inputArray[(i-2)*Nx+0];
        else if (j==(Nx2pad-2)) outputArray[i*Nx2pad+j]=inputArray[(i-2)*Nx+1];

        else outputArray[i*Nx2pad+j]=inputArray[(i-2)*Nx+(j-2)];


    }
    }
}

//
//  sumgx.c
//  
//
//  Created by Prabhat KC on 6/15/15.
//
//


double *sumgx(double g[3][3], double inputArray[], int Nx, int Ny)
{
    int i, j, k, l;
    double sum=0.0;
    double *outputArray=(double*)malloc(sizeof(double)*Ny*Nx);
    
    for (i=1; i<(Ny-1); i++)
    for (j=1; j<(Nx-1); j++)
    {
        sum=0.0;
        for (k=-1; k<2; k++)
        for (l=-1; l<2; l++)
        {
            sum+=g[k+1][l+1]*inputArray[(i+k)*Nx+(j+l)];
        }
        outputArray[i*Nx+j]=sum;
    }
    return (outputArray);
    free(outputArray);
}


