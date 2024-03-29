#version 430 core

layout(location = 0) in vec4 vPosition;
layout(location = 1) in vec3 Normal;
layout(location = 2) in vec4 Color;
layout(location = 3) in float Mode;
layout(location = 4) in float VisFlagF;


out vec4 vCol;

out vec3 vNor;

out vec3 worldPos;
out vec3 screenPos;

out float vMode;

layout(location = 0) uniform vec4 euler;
layout(location = 1) uniform vec2 screen;
layout(location = 4) uniform float scaleG;

layout(location = 5) uniform vec2 screenDelta;

layout(location = 6) uniform float VisF;


mat4 anglesToAxes(in vec3 angles)
{
    const float DEG2RAD = acos(-1) / 180.0;  // PI/180
    float sx, sy, sz, cx, cy, cz, theta;


    vec3 left, up, forward ;

    // rotation angle about X-axis (pitch)
    theta = angles.x * DEG2RAD;
    sx = sin(theta);
    cx = cos(theta);

    // rotation angle about Y-axis (yaw)
    theta = angles.y * DEG2RAD;
    sy = sin(theta);
    cy = cos(theta);

    // rotation angle about Z-axis (roll)
    theta = angles.z * DEG2RAD;
    sz = sin(theta);
    cz = cos(theta);

    // determine left axis
    left.x = cy*cz;
    left.y = sx*sy*cz + cx*sz;
    left.z = -cx*sy*cz + sx*sz;

    // determine up axis
    up.x = -cy*sz;
    up.y = -sx*sy*sz + cx*cz;
    up.z = cx*sy*sz + sx*cz;

    // determine forward axis
    forward.x = sy;
    forward.y = -sx*cy;
    forward.z = cx*cy;

    return mat4(
           vec4(left,0.0),
	   vec4(up,0.0),
	   vec4(forward,0.0),
	   vec4(0.0,0.0,0.0,1.0)
        );
}



void
main()
{

   vCol = Color;

   float scale = scaleG * euler.w;

   vNor = Normal;
   vMode = Mode;

   float n , r , t , f ;

   mat4 frustum;

   r = 1.0;
   t = 1.0;

   n = 0.1;

   f = 3;


   frustum =
     mat4( n/r , 0.0 , 0.0 , 0.0
         , 0.0 , n/t , 0.0 , 0.0
	 , 0.0 , 0.0 , (-1.0 * (f + n)) /( f - n) , (-2.0 * f * n) /( f - n)
	 , 0.0 , 0.0 , -1.0 , 0.0);

   float aspect = screen.x/screen.y; // * 0.75;

   float sx,sy;

   if(aspect > 1.0){
      sx = scale/aspect;
      sy = scale;
   } else {
      sx = scale;
      sy = scale * aspect;
   }

   highp int VisFlag = int(VisFlagF);
   highp int Vis = int(VisF);

   int VisRes = Vis & VisFlag;

   if(VisRes!=0){
      gl_Position =
		     (
		      (
			vec4(sx , sy , 0.1 , 1.0)  *
		       ( anglesToAxes(euler.xyz) *
		      (vec4(2.0 , 2.0 , 2.0 , 1.0) * ((vec4(vPosition.x , vPosition.y , vPosition.z , 1.0)
			 - vec4(0.5 , 0.5 , 0.5 , 0.0)))))))
			  + vec4(screenDelta.x,screenDelta.y,0.0,0.0) ;
   }
   else
   {
      gl_Position = vec4(0.0,0.0,0.0,0.0);
   }


   worldPos = vPosition.xyz;
   screenPos = (gl_Position.xyz)/gl_Position.w;
   

   gl_PointSize = 5.0;
}