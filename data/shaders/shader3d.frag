#version 430 core

in vec4 vCol;

in vec3 vNor;

out vec4 fColor;

void
main()
{

   vec3 lightDir = normalize(vec3(3.0 , 2.0 , 4.0));
   vec3 normal = normalize(vNor);

   float boost = 0.3 ;
   float ambient = 0.6;

   vec3 finalRGB = vCol.rgb * (ambient + boost * abs(dot(lightDir,normal)));  

   fColor = vec4(
      finalRGB   
     , vCol.a);
}