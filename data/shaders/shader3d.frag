#version 430 core

in vec4 vCol;

in vec3 vNor;

out vec4 fColor;

in vec3 worldPos;
in vec3 screenPos;

layout(location = 2) uniform double time;
layout(location = 3) uniform float shade;

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

void
main()
{

   vec3 lightDir = normalize(vec3(3.0 , 2.0 , 4.0));
   vec3 normal = normalize(vNor);

   float boost = 0.5 ;
   float ambient = 0.3;

   vec3 finalRGB;

   if (shade > 0.5){
	finalRGB = vCol.rgb * (ambient + boost * abs(dot(lightDir,normal)));  
   } else {
	finalRGB = vCol.rgb * (ambient + boost * 0.5);
   }

   // if (dotter(worldPos , 1000.0 ) < 0.5)
   // {
   //   discard;
   // }

   fColor = vec4(
      //vCol.rgb
      finalRGB   
     , vCol.a);
}