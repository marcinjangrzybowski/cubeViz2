
    var vs = `#version 300 es

in vec4 vPosition;
in vec3 Normal;
in vec4 Color;
in float Mode;
in float VisFlagF;
in float ObjGroup;

out vec4 vCol;

out vec3 vNor;

out vec3 worldPos;
out vec3 screenPos;

out float vMode;

uniform vec4 euler;
uniform vec2 screen;
uniform float scaleG;

uniform vec2 screenDelta;

uniform float VisF;
uniform mat4 poseMat;
uniform mat4 projMat;
uniform mat4 modelMat;

uniform float uTime;
uniform float uHover;

mat4 anglesToAxes(in vec3 angles)
{
    const float DEG2RAD = acos(-1.0) / 180.0;  // PI/180
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




   vec4 ColorPrim;
   bool hovered = uHover > 0.5;

   bool discardThis = false;
   // if(distance(255.0*coHover.xyz,cellCC)<1.0){
   //   hovered = true;
    
   // }else{
   //   hovered = false;

     
   // }

   if(!hovered && abs(Mode - 3.0)<0.01){
    discardThis = true;
   }

   ColorPrim = Color;

   vCol = ColorPrim;

   // vCol = coHover;//vec4(cellCC.x , cellCC.y , cellCC.z , 1.0);
   float scale = scaleG * euler.w;

   vNor = Normal;
   vMode = Mode;

   float n , r , t , f ;

   mat4 frustum;

   r = 1.0;
   t = 1.0;

   n = 0.1;

   f = 3.0;


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

   if(VisRes!=0 && !discardThis){
      gl_Position =
        (projMat)*(poseMat)*modelMat*(vec4(vPosition.x , vPosition.y , vPosition.z ,1.0));

   }
   else
   {
      gl_Position = vec4(0.0,0.0,0.0,0.0);
   }


   worldPos = vPosition.xyz;
   screenPos = (gl_Position.xyz)/gl_Position.w;


   gl_PointSize = 5.0+ObjGroup;
}
`;

    var fs = `#version 300 es
precision highp float;
in vec4 vCol;

in vec3 vNor;

in float vMode;

out vec4 fColor;

in vec3 worldPos;
in vec3 screenPos;

uniform float uTime;
uniform float shade;
uniform float slice;

uniform float alphaOverwrite;


float checker(vec3 uvw, float repeats)
{
  float cx = floor(repeats * uvw.x);
  float cy = floor(repeats * uvw.y);
  float cz = floor(repeats * uvw.z);
  float result = mod(cx + cz + cy, 2.0);
  return sign(result);
}

float dotter(vec3 uvw, float repeats)
{
  float cx = mod(floor(repeats * uvw.x) , 2.0);
  float cy = mod(floor(repeats * uvw.y) , 2.0);
  float cz = mod(floor(repeats * uvw.z) , 2.0);
  float result = cx * cz * cy;
  return sign(result);
}

float stripper(vec3 uvw, float repeats, float fill , float delta)
{
  float phase = mod(repeats * (uvw.x + uvw.y + uvw.z + delta) , 1.0);

  if(phase > fill){
	return 0.0;
  }else{
	return 1.0;
	}


}


void
main()
{

   vec3 lightDir = normalize(vec3(3.0 , 2.0 , 4.0));
   vec3 normal = normalize(vNor);

   float boost = 0.5 ;
   float ambient = 0.3;

   float allLightFactor = 1.0;

   vec3 preRGB;
   vec3 finalRGB;

   preRGB = vCol.rgb;

   float time = uTime/1000.0;

   if(vMode == 0.0){

   }else if(vMode == 1.0){
      if (stripper(worldPos , 30.0 , 0.5  , float(time*0.1) ) < 0.5)
      {
       preRGB  = vec3(0.0,0.0,0.0);
      }else{
       preRGB  = vec3(1.0,1.0,1.0);
      }
   }else if(vMode == 2.0){
      if (checker(worldPos , 10.0) < 0.5)
      {
       discard;
      }
   }else if(abs(vMode - 3.0)<0.01){ // selected primitive
      if (stripper(worldPos , 10.0 , 0.5  , float(time*0.1) ) < 0.5)
      {
       discard;
       // allLightFactor = 1.7;
      }
   }else if(vMode == 4.0){ // hollowBox
      if (stripper(worldPos , 50.0 , 0.5  , float(time*0.0) ) < 0.5)
      {
       discard;
       // allLightFactor = 1.7;
      }
   }else if(vMode == 5.0){ // stripped
      if (stripper(worldPos , 10.0 , 0.2  , float(time*0.1) ) < 0.5)
      {
       discard;
      }
   }else if(vMode == 6.0){ // cursor moving
      if (stripper(worldPos , 30.0 , 0.5  , float(time*0.1) ) < 0.5)
      {
       preRGB  = preRGB * 0.7;
      }else{
       preRGB  = preRGB * 1.3;
      }
   }else if(vMode == 7.0){ // cursor stationary
      if (stripper(worldPos , 30.0 , 0.5  , float(time*0.0) ) < 0.5)
      {
       preRGB  = vec3(0.0,0.0,0.0);
      }else{
       preRGB  = vec3(1.0,1.0,1.0);
      }
   }

   if (shade > 0.5){
	finalRGB = preRGB * allLightFactor * (ambient + boost * abs(dot(lightDir,normal)));
   } else {
	finalRGB = preRGB * allLightFactor * (ambient + boost * 0.5);
   }
   //
   // if(abs (worldPos.y - slice) > 0.02) {
   //   discard;
   // }

   // if(worldPos.x > slice) {
   //   discard;
   // }


   // if (dotter(worldPos , 1000.0 ) < 0.5)
   // {
   //   discard;
   // }
   //fColor = vec4(0.5,0.5,0.5,1.0);
   fColor = vec4(
      //vCol.rgb
      finalRGB
     , vCol.a*alphaOverwrite);
}`;
