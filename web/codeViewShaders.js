
    var vsCodeView = `#version 300 es

in vec3 vPosition;
uniform mat4 poseMat;
uniform mat4 projMat;
uniform mat4 modelMat;
out vec2 vTex;

void
main()
{
      // vCol = vColor;//vec3(1.0,0.0,0.0);//
      vTex = vec2(vPosition.x , vPosition.y);
      gl_Position =
        (projMat)*(poseMat)*modelMat*
   (vec4(vPosition.x , vPosition.y , vPosition.z ,1.0));




}
`;

    var fsCodeView = `#version 300 es
precision highp float;
// The texture.
uniform sampler2D u_texture;
uniform sampler2D u_textureMap;
uniform vec2 coCu;
uniform float uTime;
uniform vec3 selectedCC;
out vec4 fColor;
in vec2 vTex;
void
main()
{

   vec2 vTexCorrectedY = vec2(vTex.x*2.0 , 1.0 - vTex.y);
   vec2 vTexMbInv;
   

   if(gl_FrontFacing){
      vTexMbInv = vec2(1.0 - vTexCorrectedY.x , vTexCorrectedY.y);
   }else{
      vTexMbInv = vTexCorrectedY;
   } 


   vec4 co = texture(u_texture, vTexMbInv );
   vec4 coMap = texture(u_textureMap, vTexMbInv );
   vec4 coHover = texture(u_textureMap, coCu );


   float hovered;

   if(distance(selectedCC,255.0*coMap.xyz)<1.0){
     hovered = 0.5+0.5*abs(sin(uTime/200.0));
   }else{
     hovered = 0.0;
   }



   if(abs(distance(coCu*vec2(1.0,2.0),vTexMbInv*vec2(1.0,2.0)))<0.02){
     fColor = vec4(0.0,0.0,1.0,1.0);
   }else{
     fColor = co*vec4(1.0,1.0-hovered*0.5,1.0-hovered*0.5,1.0);
   }
   
   

     //vec4(step(0.5,co.x) , step(0.5,co.y) , step(0.5,co.z) , 1.0 );
// vec4(1.0,0.0,0.0,1.0);
   // fColor = vec4(
   //    //vCol.rgb
   //    finalRGB
   //   , vCol.a);
}`;
