#version 430 core

out vec4 fColor;

in vec2 MM0;
in vec2 MM1;
in vec2 MM2;
in vec2 MM3;

in vec2 vCtrl;
in vec4 vPos;

in vec4 vCol;

in vec3 vNor;

float sign3 (vec2 p1, vec2 p2, vec2 p3)
{
    return (p1.x - p3.x) * (p2.y - p3.y) - (p2.x - p3.x) * (p1.y - p3.y);
}


bool PointInTriangle (vec2 pt, vec2 v1, vec2 v2, vec2 v3)
{
    float d1, d2, d3;
    bool has_neg, has_pos;

    d1 = sign3(pt, v1, v2);
    d2 = sign3(pt, v2, v3);
    d3 = sign3(pt, v3, v1);

    has_neg = (d1 < 0) || (d2 < 0) || (d3 < 0);
    has_pos = (d1 > 0) || (d2 > 0) || (d3 > 0);

    return !(has_neg && has_pos);
}


void
main()
{
   vec4 w = gl_FragCoord;

   float mask;

   if(vCtrl.y == 0.0){
      mask = 1.0;
   }else{
      mask = (PointInTriangle( vPos.xy , MM0 , MM1 , MM2)
              ||  PointInTriangle( vPos.xy , MM1 , MM2 , MM3)) ? (1.0) : 0.0;  
   }

   vec3 lightDir = normalize(vec3(3.0 , 2.0 , 4.0));

   float boost = 2.5 ;
   float ambient = 0.4;

   vec3 finalRGB = vCol.rgb * (ambient + boost * abs(dot(lightDir,vNor)));  
   // abs(normalize(vNor));
   
// vCol.rgb
   mask = 1.0;
   fColor = vec4(
      finalRGB   
     , mask);
}