#include <stdio.h>
#include <math.h>
#include <R.h>
#include "sp.h"

/* 
   Compute surface area, using method from:

   Calculating Landscape Surface Area from Digital Elevation Models
   Author(s): Jeff S. Jenness
   Source: Wildlife Society Bulletin, Vol. 32, No. 3 (Autumn, 2004), pp. 829-839
   Published by: Allen Press
   Stable URL: http://www.jstor.org/stable/3784807

   with edge adjustments.

 (c) Barry Rowlingson 2010 <b.rowlingson@lancaster.ac.uk>

*/

double height(double *heights, int *nx, int i, int j){
  return(heights[(i)+(*nx)*(j)]);
}

double triarea(double a, double b, double c){
  /* triangle area given side lengths */
  double s;
  s=(a+b+c)/2.0;
  return(sqrt(s*(s-a)*(s-b)*(s-c)));
}

void sarea(double *heights, int *nx, int *ny, double *w, double *h, double *sa, int *bycell){

  /*
    given an *nx by *ny matrix of *heights with single-cell edge border, compute
    the surface area. If bycell==1, then return a matrix of individual cell
    area estimates, otherwise add them all up and return the sum in *sa (allocated in R)
  */

  /* point values */
  double z1,z2,z3;
  /* side lengths */
  double l1,l2,l3; 

  /* diagonal length */
  double s2 = sqrt((*w)*(*w)+(*h)*(*h));

  /* offsets to neighbours */
  int dxv[]={-1,0,1,1,1,0,-1,-1,-1};
  int dyv[]={-1,-1,-1,0,1,1,1,0,-1};

  /* triangle side lengths */
  /* first the radial sides */
  double side[]={s2,*h,s2,*w,s2,*h,s2,*w,s2};
  /* outer edges */
  double l3v[]={*w,*w,*h,*h,*w,*w,*h,*h};

  double cellArea;
  int cellI;

  if(*bycell==0){
    *sa = 0.0;
  }else{
    cellI=0; /* saves us computing 2-d array indices */
  }
  for(int j=1;j<(*ny-1);j++){
    for(int i=1;i<(*nx-1);i++){
      z1 = height(heights,nx,i,j);
      cellArea=0;
      if(!ISNA(z1)){
	for(int tri=0;tri<8;tri++){
	  z2=height(heights,nx,i+dxv[tri],j+dyv[tri]);
	  /* replace missing adjacent values with the current cell value */
	  if(ISNA(z2))z2=z1;
	  z3=height(heights,nx,i+dxv[tri+1],j+dyv[tri+1]);
	  if(ISNA(z3))z3=z1;
	  l1 = 0.5 * sqrt(side[tri]*side[tri]+(z1-z2)*(z1-z2));
	  l2 = 0.5 * sqrt(side[tri+1]*side[tri+1]+(z1-z3)*(z1-z3));
	  l3 = 0.5 * sqrt(l3v[tri]*l3v[tri]+(z2-z3)*(z2-z3));
	  cellArea += triarea(l1,l2,l3);	  
	}
      }
      if(*bycell==0){
	*sa += cellArea;
      }else{
	if(!ISNA(z1)){
	  sa[cellI]=cellArea;
	}
	cellI++;
      }

    }
  }
}


