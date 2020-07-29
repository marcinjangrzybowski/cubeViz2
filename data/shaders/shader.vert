#version 430 core

layout(location = 0) in vec4 vPosition;

layout(location = 1) in vec2 Ctrl;

layout(location = 2) in vec2 M0;
layout(location = 3) in vec2 M1;
layout(location = 4) in vec2 M2;
layout(location = 5) in vec2 M3;

layout(location = 6) in vec4 Color;

out vec2 MM0;
out vec2 MM1;
out vec2 MM2;
out vec2 MM3;

out vec2 vCtrl;

out vec4 vPos;

out vec4 vCol;

void
main()
{

   vCol = Color;

   vPos = vPosition;

   MM0 = M0;
   MM1 = M1;
   MM2 = M2;
   MM3 = M3;

   vCtrl = Ctrl;
   
   gl_Position = vec4(2.0 , 2.0 , 1.0 , 1.0) * (vPosition - vec4(0.5 , 0.5 , 0.0 , 0.0));
   
}