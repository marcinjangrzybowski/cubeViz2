#version 430 core

out vec4 fColor;

in vec2 MM0;
in vec2 MM1;
in vec2 MM2;

in vec4 vPos;

in vec4 vCol;


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

   mask = PointInTriangle( vPos.xy , MM0 , MM1 , MM2)  ? (1.0) : 0.0;  

   fColor = vec4(
      vCol.rgb
     , mask);
}