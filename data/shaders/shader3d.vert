#version 430 core

layout(location = 0) in vec4 vPosition;

layout(location = 1) in vec2 Ctrl;

layout(location = 2) in vec2 M0;
layout(location = 3) in vec2 M1;
layout(location = 4) in vec2 M2;
layout(location = 5) in vec2 M3;

layout(location = 6) in vec4 Color;

layout(location = 7) in vec3 Normal;

out vec2 MM0;
out vec2 MM1;
out vec2 MM2;
out vec2 MM3;

out vec2 vCtrl;

out vec4 vPos;

out vec4 vCol;

out vec3 vNor;

layout(location = 0) uniform vec3 euler;


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

   vNor = Normal;

   vPos = vPosition;

   MM0 = M0;
   MM1 = M1;
   MM2 = M2;
   MM3 = M3;

   vCtrl = Ctrl;

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

   gl_Position =
                 // frustum *
                  (
                  // vec4(0.0 , 0.0 ,  - 2.5 , 0.0)
		   // +
		   (
		     vec4(1.0 , 1.0 , 0.1 , 1.0)  *
		    ( anglesToAxes(euler) *
		   (vec4(1.0 , 1.0 , 1.0 , 1.0) * ((vec4(vPosition.x , vPosition.y , vPosition.z , 1.0)
		      - vec4(0.5 , 0.5 , 0.5 , 0.0)))))));
   
}