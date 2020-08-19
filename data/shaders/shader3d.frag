#version 430 core

out vec4 fColor;

in vec3 MM0;
in vec3 MM1;
in vec3 MM2;
in vec3 MM3;

in vec2 vCtrl;
in vec4 vPos;

in vec4 vCol;

in vec3 vNor;

float sign3 (vec2 p1, vec2 p2, vec2 p3)
{
    return (p1.x - p3.x) * (p2.y - p3.y) - (p2.x - p3.x) * (p1.y - p3.y);
}


bool PointInTetrahedron (vec3 pt, vec3 v1, vec3 v2, vec3 v3 , vec3 v4)
{

   

   float D0, D1, D2 ,D3 ,D4;
   mat4 m0, m1, m2 ,m3 ,m4;

   m0 = mat4( vec4( v1 , 1.0) , vec4( v2 , 1.0) , vec4( v3 , 1.0) , vec4( v4 , 1.0));
   m1 = mat4( vec4( pt , 1.0) , vec4( v2 , 1.0) , vec4( v3 , 1.0) , vec4( v4 , 1.0));
   m2 = mat4( vec4( v1 , 1.0) , vec4( pt , 1.0) , vec4( v3 , 1.0) , vec4( v4 , 1.0));
   m3 = mat4( vec4( v1 , 1.0) , vec4( v2 , 1.0) , vec4( pt , 1.0) , vec4( v4 , 1.0));
   m4 = mat4( vec4( v1 , 1.0) , vec4( v2 , 1.0) , vec4( v3 , 1.0) , vec4( pt , 1.0));

   D0 = (determinant(m0));
   D1 = (determinant(m1));
   D2 = (determinant(m2));
   D3 = (determinant(m3));
   D4 = (determinant(m4));

   bool allPos = (D1 > 0.0) && (D2 > 0.0) && (D3 > 0.0) && (D4 > 0.0);
   bool allNeg = (D1 < 0.0) && (D2 < 0.0) && (D3 < 0.0) && (D4 < 0.0);

   return allPos || allNeg;
}

bool SameSide(vec3 v1, vec3 v2, vec3 v3, vec3 v4 , vec3 p)
{
    vec3 normal = cross(v2 - v1, v3 - v1);
    float dotV4 = dot(normal, v4 - v1);
    float dotP = dot(normal, p - v1);
    return sign(dotV4) == sign(dotP);
}

bool PointInTetrahedron2(vec3 p , vec3 v1, vec3 v2, vec3 v3, vec3 v4)
{
    return SameSide(v1, v2, v3, v4, p) &&
           SameSide(v2, v3, v4, v1, p) &&
           SameSide(v3, v4, v1, v2, p) &&
           SameSide(v4, v1, v2, v3, p);               
}

void
main()
{
   vec4 w = gl_FragCoord;

   float mask;

   if(vCtrl.y == 0.0){
      mask = 1.0;
   }else{
      mask = PointInTetrahedron2( vPos.xyz , MM0 , MM1 , MM2 , MM3) ? (1.0) : 0.0;  
   }


   vec3 lightDir = normalize(vec3(3.0 , 2.0 , 4.0));
   vec3 normal = normalize(vNor);

   float boost = 0.3 ;
   float ambient = 0.6;

   vec3 finalRGB = vCol.rgb * (ambient + boost * abs(dot(lightDir,normal)));  
   // abs(normalize(vNor));

   if(mask==0.0)
     discard; 

   // vCol.rgb
   // mask = 1.0;
   fColor = vec4(
      finalRGB   
     , mask);
}