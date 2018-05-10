// Persistence of Vision Ray Tracer version 3.6/3.7 Include File
//    File: analytical_g.inc
//    Last updated: 09-Aug-2014 
//    [corrected #macro Triangle_R_out (Corner_1, Corner_2, Corner_3)
//     and       #macro Triangle_M_out (Corner_1, Corner_2, Corner_3) ]
//     
//    Description: Macros for Analytical Geometry
//
// Author: Friedrich A. Lohmueller, 2006 / 2014
// email: Friedrich.Lohmueller_at_t-online.de
// homepage: http://wwww.f-lohmueller.de
//
//------------------------------------------------------------------------

#ifndef(ANALYTICAL_G_INC_TEMP)
#declare ANALYTICAL_G_INC_TEMP = version;
#version 3.6;


#ifdef(View_POV_Include_Stack)
   #debug "including analytical_g.inc\n"
#end 

#include "math.inc"
#include "transforms.inc"
#include "functions.inc"

//------------------------------------------------------------------------
//------------------------------------------------------------------------
//---------------------------------------------------------------------------------------------
/*         Content:   
All objects are designed without any texture!
Use them by i.e.:
"object{ Vector( P ,Q , 0.05)   texture { ... }}

or with the command 
#default{ finish{ ambient 0.1 diffuse 0.9 }} 
use them i.e. like this:  
"object{ Vector( P ,Q , 0.05)  pigment { color Orange }}

Defined Macros:

Vector(Start,End,Radius)  Vector from start point "Start" to end point "End" 
                                 with the radius "Radius".
Distance_Marker(Start,End,Radius)  A double arrow between point "Start" and point "End"

Show_Wire_Box( Point1, Point2, R_line)  
Show_X(Point, R_line)     Shows x component of a point by a cylinder to the yz plane.
Show_Y(Point, R_line)     Shows y component of a point by a cylinder to the xz plane.
Show_Z(Point, R_line)     Shows z component of a point by a cylinder to the xy plane.

Show_Yxz(Point, R_line)   Shows y component and projections of x and z components of a 
                          point by a cylinder to the xz plane.

Plane_Nor(Nor_Vector,Dist,Intervall_start,Intervall_end) 
Plane_Dir(Start,M_1,M_2,Intervall_start,Intervall_end)

*/ 

//----------------------------------------------------------- macro "Vector(Start,End,Radius)"!
#macro Vector(P_start,P_end, R_Vector)  
union{

cylinder{ P_start, P_end - ( vnormalize(P_end - P_start)*9.5*R_Vector), R_Vector  }
cone    { P_end - ( vnormalize(P_end - P_start)*10*R_Vector), 3*R_Vector, P_end, 0 }
}// end of union
#end //-------------------------------------------------------------------------- end of macro

//----------------------------------------------------------- macro "Distance_Marker(Start,End,Radius)"!
#macro Distance_Marker(P_start,P_end, R_Vector)  
union{

cylinder{ P_start + ( vnormalize(P_end - P_start)*9.5*R_Vector), 
          P_end - ( vnormalize(P_end - P_start)*9.5*R_Vector), R_Vector  }
cone    { P_end - ( vnormalize(P_end - P_start)*10*R_Vector), 3*R_Vector, P_end, 0 }
cone    { P_start + ( vnormalize(P_end - P_start)*10*R_Vector), 3*R_Vector, P_start, 0 }
}// end of union
#end //-------------------------------------------------------------------------- end of macro

//------------------------------------------------ Show_Wire_Box
#macro Show_Wire_Box( Point1, Point2, R_line)  
 object{ Wire_Box( Point1, Point2, R_line, 0)} 
#end //-------------------------------------------------------- end of macro

//------------------------------------------------ macros to show the components !
#macro Show_Y(Point, R_line)              //                     macro "Show_Y(Point,Radius)"! 
 #if (Point.y != 0)
 cylinder { <Point.x,0,Point.z>, Point , R_line}
 #else
 sphere{Point , R_line} 
 #end // of "if"

#end //-------------------------------------------------------- end of macro

#macro Show_Z(Point, R_line)  
 #if (Point.z != 0)
 cylinder { <Point.x,Point.y,0>, Point , R_line}
 #else
 sphere{Point , R_line} 
 #end  // of "if"
#end  //-------------------------------------------------------- end of macro

#macro Show_X(Point, R_line)  
 #if (Point.x != 0)
 cylinder { <0,Point.y,Point.z>, Point , R_line}
 #else
 sphere{Point , R_line} 
 #end // of "if"
#end  //-------------------------------------------------------- end of macro

#macro Show_Yxz(Point, R_line)              //                     macro "Show_Yxz(Point,Radius)"! 
union{
 #if (Point.y != 0)
 cylinder { <Point.x,0,Point.z>, Point , R_line}
 #else
 sphere{Point , R_line} 
 #end // of "if"

 #if (Point.x != 0)
 cylinder { <Point.x,0,Point.z>,<0,0,Point.z> , R_line}
 #else
 sphere{Point , R_line} 
 #end // of "if"

 #if (Point.z != 0)
 cylinder { <Point.x,0,Point.z>,<Point.x,0,0> , R_line}
 #else
 sphere{Point , R_line} 
 #end // of "if"

}// end of union
#end //-------------------------------------------------------- end of macro

#macro Show_Yxy(Point, R_line)              //                     macro "Show_Yxy(Point,Radius)"! 
union{

 #if (Point.y != 0)
 cylinder { <Point.x,Point.y,0>,<Point.x,0,0>, R_line}
 #else
 sphere{Point , R_line} 
 #end // of "if"

 #if (Point.x != 0)
 cylinder { <Point.x,Point.y,0>,<0,Point.y,0> , R_line}
 #else
 sphere{Point , R_line} 
 #end // of "if"

 #if (Point.z != 0)
 cylinder { <Point.x,Point.y,0>,Point , R_line}
 #else
 sphere{Point , R_line} 
 #end // of "if"

}// end of union
#end //-------------------------------------------------------- end of macro

#macro Show_Yyz(Point, R_line)              //                     macro "Show_Yxy(Point,Radius)"! 
union{

 #if (Point.y != 0)
 cylinder { <0,Point.y,Point.z>,<0,0,Point.z> R_line}
 #else
 sphere{Point , R_line} 
 #end // of "if"

 #if (Point.x != 0)
 cylinder { <0,Point.y,Point.z>,Point , R_line}
 #else
 sphere{Point , R_line} 
 #end // of "if"

 #if (Point.z != 0)
 cylinder { <0, Point.y,Point.z>,<0,Point.y,0> , R_line}
 #else
 sphere{Point , R_line} 
 #end // of "if"

}// end of union
#end //-------------------------------------------------------- end of macro
//------------------------------------------------------------------------------------------------



  
//--------------------------------------------------------------------------- Plane in normal form
#macro Plane_Nor(Nor_Vector_,D_,Intervall_start,Intervall_end)            // normal + D (?dist*|N|)
  // Plane in normal form  normal vector n1,n2,n3>  
  // Ebene in Normalenform "n1*x1 + n2*x2 + n3*x3 + Dist = 0"
#local Nor_Vector = vnormalize(Nor_Vector_);
#local D = D_/vlength(Nor_Vector_);
#local A=Nor_Vector.x; #local B=Nor_Vector.y; #local C=Nor_Vector.z;  
#local A=Nor_Vector.x; #local B=Nor_Vector.y; #local C=Nor_Vector.z;  
isosurface {
  function {  A*x + B*y + C*z  - D }          
  contained_by { box {  Intervall_start, Intervall_end } } // container shape
  accuracy 0.001         // accuracy of calculation [0.001]
  max_gradient 60       // maximum gradient the function can have [1.1]
  open                  // remove visible container surface
}
#end 
//sample: -------------------------------------------
//object{ Plane_N(Nor_Vector, D, Intervall_start, Intervall_end) texture{ pigment{color Red}}}
//-------------------------------------------------------------------------------------------------

//--------------------------------------------------------------------------- Plane in normal form
#macro Plane_NoP(Nor_Vector,Point,Intervall_start,Intervall_end)           // normal + point
  // Plane in normal form  normal vector n1,n2,n3>  
  // Ebene in Normalenform "n1*x1 + n2*x2 + n3*x3 + Dist = 0"
#local A=Nor_Vector.x; #local B=Nor_Vector.y; #local C=Nor_Vector.z;
isosurface {
  function {  A*x + B*y + C*z  + 0  }          
  contained_by { box {  Intervall_start-Point, Intervall_end-Point } } // container shape
  accuracy 0.001         // accuracy of calculation [0.001]
  max_gradient 60       // maximum gradient the function can have [1.1]
  open                  // remove visible container surface
translate Point
}
#end 
//sample: -------------------------------------------
//object{ Plane_N(Nor_Vector, Point, Intervall_start, Intervall_end) texture{ pigment{color Red}}}
//-------------------------------------------------------------------------------------------------


//--------------------------------------------------------------------------- Plane by directions
#macro Plane_Dir(Start,M_1,M_2,Intervall_start,Intervall_end)
  // Parameterform 
  // Ebene aus Richtungsvektoren -> Normalenform "n1*x1 + n2*x2 + n3*x3+D = 0"
#local Nor_Vector = VPerp_To_Plane(M_1,M_2); //normierter Normalenvektor von O weg zeigend!!!
#local D = 0; 
#local A=Nor_Vector.x; #local B=Nor_Vector.y; #local C=Nor_Vector.z;  
isosurface {
  function {  A*x + B*y + C*z  + D  }          
  contained_by {
   box {  Intervall_start-Start, Intervall_end-Start}}// container shape
  accuracy 0.001         // accuracy of calculation [0.001]
  max_gradient 60       // maximum gradient the function can have [1.1]
  open                  // remove visible container surface
translate Start
}
#end 
//sample: -------------------------------------------
//object{ Plane_Dir(Start, M_1, M_2, Intervall_start, Intervall_end) texture{ pigment{color Red}}}
//-------------------------------------------------------------------------------------------------


//-------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------Startpunkt + Kantenvektoren
#macro Parallelepiped_by_Corners (Corner_0, Corner_1, Corner_2, Corner_3, R_Base, R_Ang, Filled) 
 #local Corner_4 = Corner_1+(Corner_2-Corner_0);  
 #local Corner_5 = Corner_1+(Corner_3-Corner_0);  
 #local Corner_6 = Corner_3+(Corner_2-Corner_0);  
 #local Corner_7 = Corner_6+(Corner_1-Corner_0);  
union{
       #if (R_Ang != 0)
       sphere  { Corner_0, R_Ang}
       sphere  { Corner_1, R_Ang}
       sphere  { Corner_2, R_Ang}
       sphere  { Corner_3, R_Ang}
       sphere  { Corner_4, R_Ang}
       sphere  { Corner_5, R_Ang}
       sphere  { Corner_6, R_Ang}
       sphere  { Corner_7, R_Ang}
       #end
               
       #if (R_Base != 0)
       cylinder{ Corner_0, Corner_1, R_Base }
       cylinder{ Corner_0, Corner_2, R_Base }
       cylinder{ Corner_0, Corner_3, R_Base }
       cylinder{ Corner_1, Corner_4, R_Base }
       cylinder{ Corner_1, Corner_5, R_Base }
       cylinder{ Corner_2, Corner_4, R_Base }
       cylinder{ Corner_2, Corner_6, R_Base }
       cylinder{ Corner_3, Corner_6, R_Base }
       cylinder{ Corner_3, Corner_5, R_Base }
       cylinder{ Corner_4, Corner_7, R_Base }
       cylinder{ Corner_5, Corner_7, R_Base }
       cylinder{ Corner_6, Corner_7, R_Base }
       #end
               
        #if (Filled = 1) 
        triangle{ Corner_0, Corner_1, Corner_2 }
        triangle{ Corner_1, Corner_2, Corner_4 }

        triangle{ Corner_0, Corner_3, Corner_2 }
        triangle{ Corner_2, Corner_3, Corner_6 }

        triangle{ Corner_0, Corner_1, Corner_5 }
        triangle{ Corner_0, Corner_5, Corner_3 }
       
        triangle{ Corner_1, Corner_5, Corner_7 }
        triangle{ Corner_1, Corner_7, Corner_4 }

        triangle{ Corner_2, Corner_4, Corner_7 }
        triangle{ Corner_2, Corner_7, Corner_6 }

        triangle{ Corner_3, Corner_5, Corner_7 }
        triangle{ Corner_3, Corner_7, Corner_6 }
        #end // of "if (Filled = 1)" 

  }// end of union
#end //-------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------
#macro Parallelflach_durch_Ecken (Point_P0d_, Corner_V1d_, Corner_V2d_, Corner_V3d_, R_Based_, R_Angd_, Filled_) 
 object{ Parallelepiped_by_Corners (Point_P0d_, Corner_V1d_, Corner_V2d_, Corner_V3d_, R_Based_, R_Angd_, Filled_)} 
#end //-------------------------------------------------------------------------------------------------
#macro Spat_durch_Ecken (Point_P0d_, Corner_V1d_, Corner_V2d_, Corner_V3d_, R_Based_, R_Angd_, Filled_) 
 object{ Parallelepiped_by_Corners (Point_P0d_, Corner_V1d_, Corner_V2d_, Corner_V3d_, R_Based_, R_Angd_, Filled_)} 
#end //-------------------------------------------------------------------------------------------------


//-------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------Startpunkt + Kantenvektoren
#macro Parallelepiped_by_Edges (Point_P0, Edge_V1, Edge_V2, Edge_V3, R_Base, R_Ang, Filled) 
 
union{
       #if (R_Ang != 0)
       sphere  { o, R_Ang}
       sphere  { Edge_V1, R_Ang}
       sphere  { Edge_V2, R_Ang}
       sphere  { Edge_V3, R_Ang}
       sphere  { Edge_V1+Edge_V2, R_Ang}
       sphere  { Edge_V1+Edge_V3, R_Ang}
       sphere  { Edge_V2+Edge_V3, R_Ang}
       sphere  { Edge_V1+Edge_V2+Edge_V3, R_Ang}
       #end
               
       #if (R_Base != 0)
       cylinder{ o, Edge_V1, R_Base }
       cylinder{ o, Edge_V2, R_Base }
       cylinder{ o, Edge_V3, R_Base }

       cylinder{ Edge_V1, Edge_V1+Edge_V2, R_Base }
       cylinder{ Edge_V1, Edge_V1+Edge_V3, R_Base }

       cylinder{ Edge_V2, Edge_V2+Edge_V1, R_Base }
       cylinder{ Edge_V2, Edge_V2+Edge_V3, R_Base }

       cylinder{ Edge_V3, Edge_V3+Edge_V1, R_Base }
       cylinder{ Edge_V3, Edge_V3+Edge_V2, R_Base }

       cylinder{ Edge_V1+Edge_V2,Edge_V1+Edge_V2+Edge_V3, R_Base }
       cylinder{ Edge_V1+Edge_V3,Edge_V1+Edge_V2+Edge_V3, R_Base }
       cylinder{ Edge_V2+Edge_V3,Edge_V1+Edge_V2+Edge_V3, R_Base }
       #end
               
        #if (Filled = 1) 
        triangle{ o, Edge_V1, Edge_V2 }
        triangle{ Edge_V1, Edge_V2, Edge_V1+Edge_V2 }
        
        triangle{ Edge_V3, Edge_V1+Edge_V3, Edge_V2+Edge_V3 }
        triangle{ Edge_V1+Edge_V3, Edge_V2+Edge_V3, Edge_V1+Edge_V2+Edge_V3 }

        triangle{ o, Edge_V2, Edge_V3 }
        triangle{ Edge_V2, Edge_V3, Edge_V2+Edge_V3 }
        triangle{ Edge_V1, Edge_V2+Edge_V1, Edge_V3+Edge_V1 }
        triangle{ Edge_V2+Edge_V1, Edge_V3+Edge_V1, Edge_V2+Edge_V3+Edge_V1 }

        triangle{ o, Edge_V3, Edge_V1 }
        triangle{ Edge_V3, Edge_V1, Edge_V3+Edge_V1 }
        triangle{ Edge_V2, Edge_V3+Edge_V2, Edge_V1+Edge_V2 }
        triangle{ Edge_V3+Edge_V2, Edge_V1+Edge_V2, Edge_V3+Edge_V1+Edge_V2 }

        #end // of "if (Filled = 1)" 
  
  translate Point_P0
  }// end of union
#end //-------------------------------------------------------------------------------------------------
//------------------------------------------------------------------------------------------------------
#macro Parallelflach_durch_Kanten (Point_P0d_, Edge_V1d_, Edge_V2d_, Edge_V3d_, R_Based_, R_Angd_, Filled_) 
 object{ Parallelepiped_by_Edges (Point_P0d_, Edge_V1d_, Edge_V2d_, Edge_V3d_, R_Based_, R_Angd_, Filled_)} 
#end //-------------------------------------------------------------------------------------------------
#macro Spat_durch_Kanten (Point_P0d_, Edge_V1d_, Edge_V2d_, Edge_V3d_, R_Based_, R_Angd_, Filled_) 
 object{ Parallelepiped (Point_P0d_, Edge_V1d_, Edge_V2d_, Edge_V3d_, R_Based_, R_Angd_, Filled_)} 
#end //-------------------------------------------------------------------------------------------------


#macro Tetrahedron_by_Corners(Corner_V0, Corner_V1, Corner_V2, Corner_V3, R_Base, R_Ang, Filled)
union{ 
       #if (R_Ang != 0)
       sphere  { Corner_V0, R_Ang}
       sphere  { Corner_V1, R_Ang}
       sphere  { Corner_V2, R_Ang}
       sphere  { Corner_V3, R_Ang}
       #end 
       
       #if (R_Base != 0)
       cylinder{ Corner_V0, Corner_V1, R_Base }
       cylinder{ Corner_V0, Corner_V2, R_Base }
       cylinder{ Corner_V0, Corner_V3, R_Base }

       cylinder{ Corner_V1, Corner_V2, R_Base }
       cylinder{ Corner_V2, Corner_V3, R_Base }
       cylinder{ Corner_V3, Corner_V1, R_Base }
       #end  
       
       #if (Filled = 1) 
       triangle{ Corner_V0, Corner_V1, Corner_V2 }
       triangle{ Corner_V0, Corner_V2, Corner_V3 }
       triangle{ Corner_V0, Corner_V3, Corner_V1 }
       triangle{ Corner_V3, Corner_V1, Corner_V2 }
       #end // of "if (Filled = 1)" 

  }// end of union
#end //-------------------------------------------------------------------------


//-------------------------------------------------------------- startpoint and vectors of the edges
//-------------------------------------------------------------- Startpunkt und Kantenvektoren
#macro Tetrahedron_by_Edges(Point_0, Edge_V1, Edge_V2, Edge_V3, R_Base, R_Ang, Filled) 
union{ 
       #if (R_Ang != 0)
       sphere  { o, R_Ang}
       sphere  { Edge_V1, R_Ang}
       sphere  { Edge_V2, R_Ang}
       sphere  { Edge_V3, R_Ang}
       #end 
       
       #if (R_Base != 0)
       cylinder{ o, Edge_V1, R_Base }
       cylinder{ o, Edge_V2, R_Base }
       cylinder{ o, Edge_V3, R_Base }
       cylinder{ Edge_V1, Edge_V2, R_Base }
       cylinder{ Edge_V1, Edge_V3, R_Base }
       cylinder{ Edge_V2, Edge_V3, R_Base }
       #end
             
       #if (Filled = 1) 
       triangle{ o, Edge_V1, Edge_V2 }
       triangle{ o, Edge_V2, Edge_V3 }
       triangle{ o, Edge_V3, Edge_V1 }
       triangle{ Edge_V1, Edge_V2, Edge_V3 }
       #end // of "if" 

  translate Point_0
  }// end of union
#end //------------------------------------------------------------------------------------------
//-----------------------------------------------------------------------------------------------
#macro Tetraeder (Point_0d_, Edge_V1d_, Edge_V2d_, Edge_V3d_, R_Based_, R_Angd_, Filled_) 
 object{ Tetrahedron (Point_0d_, Edge_V1d_, Edge_V2d_, Edge_V3d_, R_Based_, R_Angd_, Filled_)} 
#end //------------------------------------------------------------------------------------------





//-------------------------------------------------------------------------------
//------------------------------------------------------------------------------- circle
//------------------------------------------------------------------------------- Kreis
// circle line of a circle 
#macro Circle_Line(C_Middle, C_Radius, Line_Radius, C_Normal)
 torus{ C_Radius, Line_Radius Reorient_Trans(y,C_Normal) translate C_Middle  } 
#end  
// surface of a circle 
#macro Circle_Disc(C_Middle, C_Radius, C_Normal)
 disc { o,y, C_Radius         Reorient_Trans(y,C_Normal) translate C_Middle  } 
#end

// circle line of the outer circle of a triangle   
#macro Circle_Line_out(Corner_1, Corner_2, Corner_3, Line_Radius)
 #local Middle =  Triangle_M_out (Corner_1, Corner_2, Corner_3);
 #local Radius =  Triangle_R_out (Corner_1, Corner_2, Corner_3);
 #local Normal =  Triangle_Normal(Corner_1, Corner_2, Corner_3);
 Circle_Line ( Middle, Radius, Line_Radius, Normal)
#end

// surface of the outer circle of a triangle
#macro Circle_Disc_out(Corner_1, Corner_2, Corner_3)
 #local Middle =  Triangle_M_out (Corner_1, Corner_2, Corner_3);
 #local Radius =  Triangle_R_out (Corner_1, Corner_2, Corner_3);
 #local Normal =  Triangle_Normal(Corner_1, Corner_2, Corner_3);
 disc { o,y, Radius  Reorient_Trans(y,Normal) translate Middle } 
#end

// circle line of the inner circle of a triangle   
#macro Circle_Line_in (Corner_1, Corner_2, Corner_3, Line_Radius)
 #local Middle =  Triangle_M_in (Corner_1, Corner_2, Corner_3);
 #local Radius =  Triangle_R_in (Corner_1, Corner_2, Corner_3);
 #local Normal =  Triangle_Normal(Corner_1, Corner_2, Corner_3);
 Circle_Line ( Middle, Radius, Line_Radius, Normal)
#end
// surface of the inner circle of a triangle
#macro Circle_Disc_in (Corner_1, Corner_2, Corner_3)
 #local Middle =  Triangle_M_in (Corner_1, Corner_2, Corner_3);
 #local Radius =  Triangle_R_in (Corner_1, Corner_2, Corner_3);
 #local Normal =  Triangle_Normal(Corner_1, Corner_2, Corner_3);
 disc { o,y, Radius  Reorient_Trans(y,Normal) translate Middle } 
#end


#macro Triangle_Normal(Corner_1, Corner_2, Corner_3)
  vnormalize(vcross((Corner_2-Corner_1),(Corner_3-Corner_1)))
#end    
         
// Center of Mass_Center of the triangle
#macro Triangle_Mass_Center (Corner_1, Corner_2, Corner_3)
  (Corner_1+Corner_2+Corner_3)/3
#end    


// Center M of the circumcircle 
#macro Triangle_M_out (Corner_1, Corner_2, Corner_3)
#local C1 = Corner_1;
#local C2 = Corner_2;
#local C3 = Corner_3;

#if( VAngleD( C1-C2,C3-C2) < VAngleD( C3-C1,C3-C2))
#local C1 = Corner_1;
#local C2 = Corner_3;
#local C3 = Corner_2;
#end // #if

   (C1+C2)/2 +sqrt( Triangle_R_out(C1,C2,C3)*Triangle_R_out(C1,C2,C3) - 1/4*vlength(C2-C1)*vlength(C2-C1))
   * ( vcross( Triangle_Normal(C1,C2,C3), vnormalize(C2-C1)) )
#end    


         
// Radius R_out of the circumcircle circle                               
#macro Triangle_R_out (Corner_1, Corner_2, Corner_3)
#local C1 = Corner_1;
#local C2 = Corner_2;
#local C3 = Corner_3;

#if( VAngleD( C1-C2,C3-C2) < VAngleD( C3-C1,C3-C2))
#local C1 = Corner_1;
#local C2 = Corner_3;
#local C3 = Corner_2;
#end // #if

  #local V1 = vnormalize(C2-C1);
  #local V2 = vnormalize(C3-C1);
  #local V3 = C3-C2;
  #local Sinus = vlength( vcross( V1, V2));
  vlength(V3)/(2*Sinus)
#end  

// end new part

// Area A of the triangle
#macro Triangle_Area (Corner_1, Corner_2, Corner_3)
  vlength( vcross( (Corner_2-Corner_1),(Corner_3-Corner_1) ) )/2
#end      
                        
// Circumference of the triangle                        
#macro Triangle_Circum (Corner_1, Corner_2, Corner_3)
  #local V1 = Corner_2-Corner_1;
  #local V2 = Corner_3-Corner_1;
  #local V3 = Corner_3-Corner_2;
  (vlength(V1) + vlength(V2) + vlength(V3))
#end      
                                
// radius r_inner of the inner circle                               
#macro Triangle_R_in (Corner_1, Corner_2, Corner_3)
  2*Triangle_Area(Corner_1, Corner_2, Corner_3)/Triangle_Circum(Corner_1, Corner_2, Corner_3)
#end       
                            
// center M_in of the inner circle                                                            
#macro Triangle_M_in (Corner_1, Corner_2, Corner_3)
  #local V1 = Corner_2-Corner_1 ;
  #local V2 = Corner_3-Corner_1 ;
  #local V3 = Corner_3-Corner_2 ;
  #local R_in  = Triangle_R_in( Corner_1, Corner_2, Corner_3) ;
  #local T     = vnormalize( vnormalize(V1) + vnormalize(V2) );
  #local Sinus = vlength( vcross( T , vnormalize(V1) ) );
  Corner_1 + R_in/Sinus * T 
#end      
                



//-------------------------------------------------------------------------------
#version ANALYTICAL_G_INC_TEMP;
#end
//--------------------------------------------------------------------------- end



