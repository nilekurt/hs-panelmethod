#version 310 es

layout(location = 0) in vec2 in_Position;
uniform mat4 transform;

void main()
{
   gl_Position = transform * vec4(in_Position, 0.0, 1.0);
}
