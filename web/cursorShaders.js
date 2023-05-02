
    var vsCursor = `#version 300 es

in vec3 vPosition;
in vec3 vColor;

out vec3 vCol;
uniform mat4 poseMat;
uniform mat4 projMat;
uniform mat4 cuMat;

void
main()
{
      vCol = vColor;//vec3(1.0,0.0,0.0);//
    
      gl_Position =
        (projMat)*(poseMat)*cuMat*
   (vec4(0.1,0.1,0.1,1.0)*vec4(vPosition.x , vPosition.y , vPosition.z ,1.0));




}
`;

    var fsCursor = `#version 300 es
precision highp float;
out vec4 fColor;
in vec3 vCol;

void
main()
{


   fColor = vec4(vCol.x,vCol.y,vCol.z,1.0);
   // fColor = vec4(
   //    //vCol.rgb
   //    finalRGB
   //   , vCol.a);
}`;
