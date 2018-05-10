//------------------------------------------------------------------------
#version 3.7;
global_settings{ assumed_gamma 1.0 }
//------------------------------------------------------------------------
#include "colors.inc"
#include "textures.inc"
#include "glass.inc"
#include "metals.inc"
#include "golds.inc"
#include "stones.inc"
#include "woods.inc"
#include "shapes.inc"
#include "shapes2.inc"
#include "functions.inc"
#include "math.inc"          
#include "transforms.inc"
//------------------------------------------------------------------------
#declare Camera_0 = camera {                                 // xy-view
                             angle 20
                             location  <0.0 , 0.0 ,-38.0>
                             right     x*image_width/image_height
                             look_at   <0.0, 0.0 , 0.0>
                           }

#declare Camera_1 = camera {                                // diagonal view
                             angle 9
                             location  <30.0 ,15.5 , 40.0>*1.2
                             right     x*image_width/image_height
                             look_at   <0.0 , 0.0 ,0.0>
                           }
#declare Camera_2 = camera {                               // yz-view
                             angle 20          
                             location  <31.0 , 7.0 ,1.0>
                             right     x*image_width/image_height
                             look_at   <1.5 , 1.4 , 1.0>
                           }
#declare Camera_3 = camera { 
                             angle 20
                             location  < 0.0 , 37.5 ,-0.0001>
                             right     x*image_width/image_height
                             look_at   <0 , 0 , 2.0>
                           }

camera{Camera_1}

//------------------------------------------------------------------------
// sun -------------------------------------------------------------------
light_source{<3000,5500,-1000> color White}

// sky -------------------------------------------------------------------
sky_sphere{ pigment{ gradient <0,1,0>
                     color_map{ [0   color rgb<1,1,1>         ]//White
                                [0.4 color rgb<1,1,1>]//~Navy
                                [0.6 color rgb<1,1,1>]//<0.14,0.14,0.56>]//~Navy
                                [1.0 color rgb<1,1,1>         ]//White
                              }
                     scale 2 }
           } // end of sky_sphere 
//------------------------------------------------------------------------
// 
//------------------------------------------------------------------------------------
#include "analytical_g.inc" 

//------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------- 
#default{ finish {ambient 0.15 diffuse 0.85} } // 


// spheres.pov 
//-----------------------------------------------------------------------------------
#declare R1  = 2.0;   
#declare R2  = 4.0;

#declare Rl  = 0.055; // radius of lines  - Radius der Strecken 
#declare Rp  = 0.105; // radius of points - Radius der Punkte 


#macro WireFrameSphere(NrLongitudes, NrLatitudes, Rmaj, Rmin)

  #local dLongitude = 360/NrLongitudes;
  #local dLatitude = 180/NrLatitudes;
  #local Cnt = 0;
  #while (Cnt < NrLongitudes)
    #local Longitude = Cnt*dLongitude;
    difference {
      torus { Rmaj, Rmin }
      plane { -z, 0 }
      rotate -90*z
      rotate Longitude*y
    }
    #local Cnt = Cnt + 1;
  #end // while

  #local Cnt = 1;
  #while (Cnt < NrLatitudes)
    #local Latitude = radians(Cnt*dLatitude - 90);
    torus {
      Rmaj*cos(Latitude), Rmin
      translate Rmaj*sin(Latitude)*y
    }
    #local Cnt = Cnt + 1;
  #end // while

#end // macro WireFrameSphere

#declare Rglobe = 2.0;
#declare Rwireframe = 0.0033;
#declare Rspheres = Rwireframe*2;

// Number of longitude intervals
#declare Longitudes = 8;

// Number of latitude intervals
#declare Latitudes = 6;

// axes.pov file

//------------------------------ the Axes --------------------------------
//------------------------------------------------------------------------
#macro Axis_( AxisLen, Dark_Texture,Light_Texture) 
 union{
    cylinder { <0,0,0>,<0,AxisLen,0>,0.05
               texture{checker texture{Dark_Texture } 
                               texture{Light_Texture}
                       scale <0.3,0.3,0.3> 
                       translate<0.1,0,0.1>}
             }
    cone{<0,AxisLen,0>,0.2,<0,AxisLen+0.7,0>,0
          texture{Dark_Texture}
         }
     } // end of union                   
#end // of macro "Axis()"
//------------------------------------------------------------------------

//------------------------------ the Axes --------------------------------
//------------------------------------------------------------------------
#macro AxisB_( AxisLen, Dark_Texture,Light_Texture, Scl) 
 union{
 	 cylinder { <0,0,0>,<0,R1*Scl,0>,0.02
               texture{checker texture{Dark_Texture } 
                               texture{Light_Texture}
                       scale <0.3,0.3,0.3> 
                       translate<0.1,0,0.1>}
             }
      sphere { <0,R1*Scl,0>,0.14
               texture{Light_Texture} 
      }
      cylinder { <0,R1*Scl,0>,<0,R1*Scl+AxisLen,0>,0.05
               texture{checker texture{Dark_Texture } 
                               texture{Light_Texture}
                       scale <0.3,0.3,0.3> 
                       translate<0.1,0,0.1>}
             }
    cone{<0,R1*Scl+AxisLen,0>,0.2,<0,R1*Scl+AxisLen+0.7,0>,0
          texture{Dark_Texture}
         }
     } // end of union                   
#end // of macro "Axis()"
//------------------------------------------------------------------------



#macro AxisXYZ( AxisLenX, AxisLenY, AxisLenZ, Tex_X, Tex_Y, Tex_Z)
//--------------------- drawing of 3 Axes --------------------------------
union{
#if (AxisLenX != 0)
 object { Axis_(AxisLenX, Tex_X, Tex_X)   rotate< 0,0,-90>}// x-Axis
#end // of #if 
#if (AxisLenY != 0)
 object { Axis_(AxisLenY, Tex_Y, Tex_Y)   rotate< 0,0,  0>}// y-Axis
#end // of #if 
#if (AxisLenZ != 0)
 object { Axis_(AxisLenZ, Tex_Z, Tex_Z)   rotate<90,0,  0>}// z-Axis
#end // of #if 
} // end of union
#end// of macro "AxisXYZ( ... )"
//------------------------------------------------------------------------

//------------------------------------------------------------------------
#macro AxisXYZgrainA( AxisLenX, AxisLenY, AxisLenZ, Tex_X, Tex_Y, Tex_Z)
//--------------------- drawing of 3 Axes --------------------------------
union{
 object { Axis_(AxisLenX, Tex_X, Tex_X)  scale <0.5,0.5,0.5>  rotate< 0,0,-90>}// x-Axis
 object { Axis_(AxisLenY, Tex_Y, Tex_Y)  scale <0.5, 0.5, 0.5> rotate< 0,0,  0>}// y-Axis
 object { Axis_(AxisLenZ, Tex_Z, Tex_Z)  scale <0.5,0.5,0.5>  rotate<90,0,  0>}// z-Axis
// This is the central sphere representing grain A.
 sphere {
	<0.0, 0.0, 0.0>, R1
	material {
		texture {
			pigment {
				rgbt <0.06804, 0.8351, 0.300000, 0.75>
			}
			finish {
				diffuse 0.6, 0.6
				brilliance 1.0
			}
		}
	}
 }
 union {
  WireFrameSphere(Longitudes, Latitudes, Rglobe, Rwireframe)
  pigment { color Black*0.7 }
 }
} // end of union
#end// of macro "AxisXYZgrainA( ... )"

//------------------------------------------------------------------------
#macro AxisXYZgrainB( AxisLenX, AxisLenY, AxisLenZ, Tex_X, Tex_Y, Tex_Z, Scl)
//--------------------- drawing of 3 Axes --------------------------------
union{
 object { AxisB_(AxisLenX, Tex_X, Tex_X, Scl)  scale <0.5,0.5,0.5>  rotate< 0,0,-90>}// x-Axis
 object { AxisB_(AxisLenY, Tex_Y, Tex_Y, Scl)  scale <0.5, 0.5, 0.5> rotate< 0,0,  0>}// y-Axis
 object { AxisB_(AxisLenZ, Tex_Z, Tex_Z, Scl)  scale <0.5,0.5,0.5>  rotate<90,0,  0>}// z-Axis
  // add the sphere
 sphere {
	<0.0, 0.0, 0.0>, R2
	material {
		texture {
			pigment {
				rgbt <0.016804, 0.198351, 1.000000, 0.85>
			}
			finish {
				diffuse 0.6, 0.6
				brilliance 1.0
			}
		}
	}
}
 union {
  WireFrameSphere(Longitudes, Latitudes, 2*Rglobe, Rwireframe)
  pigment { color Black*0.7 }
}
} // end of union
#end// of macro "AxisXYZgrainB( ... )"

#declare Texture_A_Dark  = texture {
                               pigment{color rgb<1,0.45,0>}
                               finish {ambient 0.15 diffuse 0.85 phong 1}
                             }
#declare Texture_A_Light = texture { 
                               pigment{color rgb<1,1,1>}
                               finish {ambient 0.15 diffuse 0.85 phong 1}
                             }

#declare Texture_A_Red  = texture {
                               pigment{color rgb<1,0.0,0>}
                               finish {ambient 0.15 diffuse 0.85 phong 1}
                             }
#declare Texture_A_Green = texture { 
                               pigment{color rgb<0,1,0>}
                               finish {ambient 0.15 diffuse 0.85 phong 1}
                             }
#declare Texture_A_Blue  = texture {
                               pigment{color rgb<0,0,1>}
                               finish {ambient 0.15 diffuse 0.85 phong 1}
                             }


// cubes.pov file 
//------------------------------------------------------------------------
#macro AxisXYZcubeA( AxisLenX, AxisLenY, AxisLenZ, Tex_X, Tex_Y, Tex_Z)
//--------------------- drawing of 3 Axes --------------------------------
union{
 object { Axis_(AxisLenX, Tex_X, Tex_X)  scale <0.5,0.5,0.5>  rotate< 0,0,-90> translate <-R1, -R1, -R1> }// x-Axis
 object { Axis_(AxisLenY, Tex_Y, Tex_Y)  scale <0.5, 0.5, 0.5> rotate< 0,0,  0> translate <-R1, -R1, -R1> }// y-Axis
 object { Axis_(AxisLenZ, Tex_Z, Tex_Z)  scale <0.5,0.5,0.5>  rotate<90,0,  0> translate <-R1, -R1, -R1> }// z-Axis
// This is the central cube representing grain A.
 union {
	 box {
		<-R1, -R1, -R1>, <R1, R1, R1>
		material {
			texture {
				pigment {
					rgbt <0.06804, 0.8351, 0.300000, 0.75>
				}
				finish {
					diffuse 0.6, 0.6
					brilliance 1.0
					}
				}
			}
 		}
// thin outline of the box, to make it easier to spot its rotation
	cylinder { <-R1, -R1, -R1>, < -R1, -R1,  R1>, 0.005 pigment{color Black} }
	cylinder { < R1, -R1, -R1>, <  R1, -R1,  R1>, 0.005 pigment{color Black} }
	cylinder { <-R1,  R1, -R1>, < -R1,  R1,  R1>, 0.005 pigment{color Black} }
	cylinder { < R1,  R1, -R1>, <  R1,  R1,  R1>, 0.005 pigment{color Black} }

	cylinder { <-R1, -R1, -R1>, < -R1,  R1, -R1>, 0.005 pigment{color Black} }
	cylinder { <-R1,  R1, -R1>, <  R1,  R1, -R1>, 0.005 pigment{color Black} }
	cylinder { < R1,  R1, -R1>, <  R1, -R1, -R1>, 0.005 pigment{color Black} }
	cylinder { < R1, -R1, -R1>, < -R1, -R1, -R1>, 0.005 pigment{color Black} }

	cylinder { <-R1, -R1,  R1>, < -R1,  R1,  R1>, 0.005 pigment{color Black} }
	cylinder { <-R1,  R1,  R1>, <  R1,  R1,  R1>, 0.005 pigment{color Black} }
	cylinder { < R1,  R1,  R1>, <  R1, -R1,  R1>, 0.005 pigment{color Black} }
	cylinder { < R1, -R1,  R1>, < -R1, -R1,  R1>, 0.005 pigment{color Black} }
	}
} // end of union
#end// of macro "AxisXYZcubeA( ... )"

//------------------------------------------------------------------------
#macro AxisXYZcubeB( AxisLenX, AxisLenY, AxisLenZ, Tex_X, Tex_Y, Tex_Z, Scl)
//--------------------- drawing of 3 Axes --------------------------------
union{
 object { AxisB_(AxisLenX, Tex_X, Tex_X, Scl)  scale <0.5,0.5,0.5>  rotate< 0,0,-90>}// x-Axis
 object { AxisB_(AxisLenY, Tex_Y, Tex_Y, Scl)  scale <0.5, 0.5, 0.5> rotate< 0,0,  0>}// y-Axis
 object { AxisB_(AxisLenZ, Tex_Z, Tex_Z, Scl)  scale <0.5,0.5,0.5>  rotate<90,0,  0>}// z-Axis
  // add the box representing grain B
 union {
 	box {
		<-R1, -R1, -R1>, <R1, R1, R1>
		material {
			texture {
				pigment {
					rgbt <0.016804, 0.198351, 1.000000, 0.85>
				}
				finish {
					diffuse 0.6, 0.6
					brilliance 1.0
					}
				}
			}
		}
// thin outline of the box, to make it easier to spot its rotation
	cylinder { <-R1, -R1, -R1>, < -R1, -R1,  R1>, 0.005 pigment{color Black} }
	cylinder { < R1, -R1, -R1>, <  R1, -R1,  R1>, 0.005 pigment{color Black} }
	cylinder { <-R1,  R1, -R1>, < -R1,  R1,  R1>, 0.005 pigment{color Black} }
	cylinder { < R1,  R1, -R1>, <  R1,  R1,  R1>, 0.005 pigment{color Black} }

	cylinder { <-R1, -R1, -R1>, < -R1,  R1, -R1>, 0.005 pigment{color Black} }
	cylinder { <-R1,  R1, -R1>, <  R1,  R1, -R1>, 0.005 pigment{color Black} }
	cylinder { < R1,  R1, -R1>, <  R1, -R1, -R1>, 0.005 pigment{color Black} }
	cylinder { < R1, -R1, -R1>, < -R1, -R1, -R1>, 0.005 pigment{color Black} }

	cylinder { <-R1, -R1,  R1>, < -R1,  R1,  R1>, 0.005 pigment{color Black} }
	cylinder { <-R1,  R1,  R1>, <  R1,  R1,  R1>, 0.005 pigment{color Black} }
	cylinder { < R1,  R1,  R1>, <  R1, -R1,  R1>, 0.005 pigment{color Black} }
	cylinder { < R1, -R1,  R1>, < -R1, -R1,  R1>, 0.005 pigment{color Black} }
	}
} // end of union
#end// of macro "AxisXYZcubeB( ... )"


//------------------------------------------------------------------------
#macro TruncationA() 
//--------------------- drawing of 3 Axes --------------------------------
box { 
	<-R2, 0.0, -R2>, <R2, R2, R2>	
	material {
		texture {
			pigment {
				rgbt <0.016804, 0.198351, 1.000000, 0.85>
			}
			finish {
				diffuse 0.6, 0.6
				brilliance 1.0
			}
		}
	}
} 
#end// of macro "TruncationA( ... )"

//------------------------------------------------------------------------
#macro TruncationB() 
//--------------------- drawing of 3 Axes --------------------------------
box { 
	<-R2, -R2, -R2>, <R2, 0.0, R2>
	material {
		texture {
			pigment {
				rgbt <0.016804, 0.198351, 1.000000, 0.85>
			}
			finish {
				diffuse 0.6, 0.6
				brilliance 1.0
			}
		}
	}
} 
#end// of macro "TruncationB( ... )"

// arrows.pov file

#declare bx = 0.5; 
#declare dx = 0.15; 
#declare normals = union {
     cylinder { <0,0,-R1/2>,<0,0,R1/2>,0.015 pigment{color Yellow} rotate 90*x}
     box { <-bx,-bx,-0.0001>,<bx,bx,0.0001> pigment{color Yellow}rotate 90*x}
     cone{<0,0,-R1/2>,0.05,<0,0,-(R1/2+dx)>,0   pigment{color Yellow} rotate 90*x}
     cone{<0,0,R1/2>,0.05,<0,0,R1/2+dx>,0   pigment{color Yellow} rotate 90*x}
}


