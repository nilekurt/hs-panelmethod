/*  This file is part of hs-panelmethod.

    hs-panelmethod is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    hs-panelmethod is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with hs-panelmethod.  If not, see <http://www.gnu.org/licenses/>. */

// VERTEX SHADER

#version 310 es

layout(location = 0) in vec2 in_Position;
uniform mat4 transform;

void main()
{
   gl_Position = transform * vec4(in_Position, 0.0, 1.0);
}
