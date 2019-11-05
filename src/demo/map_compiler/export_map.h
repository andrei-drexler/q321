#pragma once

bool ExportMap(const Q3::Map& map, const char* path) {
	if (map.entities.empty())
		return false;

	FILE* file = fopen(path, "w");
	if (!file)
		return false;
	auto close_file = scope_exit { fclose(file); file = NULL; };

	for (auto& ent : map.entities) {
		size_t ent_index = &ent - map.entities.data();
		if (ent_index > 0)
			fprintf(file, "// entity %zd\n", ent_index);
		fprintf(file, "{\n");

		for (auto& p : ent.props)
			fprintf(file, "\"%s\" \"%s\"\n", p.first.c_str(), p.second.c_str());

		for (auto& brush : ent.brushes) {
			size_t brush_index = &brush - ent.brushes.data();
			fprintf(file, "// brush %zd\n", brush_index);
			fprintf(file, "{\n");

			const size_t MAX_NUM_EDGES = 256;
			BrushEdge edges[MAX_NUM_EDGES];

			size_t num_edges = EnumerateBrushEdges(brush.planes.data(), brush.planes.size(), edges, MAX_NUM_EDGES);

			u32 edge_mask[MAX_NUM_EDGES];
			for (size_t i = 0; i < num_edges; ++i) {
				auto& e = edges[i];
				e.first_point = floor(e.first_point + 0.5f);
				e.second_point = floor(e.second_point + 0.5f);

				if (length_squared(e.first_point - e.second_point) < 0.25f)
					edge_mask[i] = 0;
				else
					edge_mask[i] = (1 << e.first_plane) | (1 << e.second_plane);
			}

			for (size_t i = 0; i < brush.planes.size(); ++i) {
				auto& plane = brush.planes[i];
				
				const size_t MAX_FACE_EDGES = 256;
				u8 face_edges[MAX_FACE_EDGES];
				size_t num_face_edges = 0;

				u32 face_mask = 1 << i;
				for (size_t j = 0; j < num_edges; ++j)
					if (edge_mask[j] & face_mask)
						face_edges[num_face_edges++] = j;

				if (num_face_edges < 3)
					continue;
#if 0
				vec3 center = 0.f;
				for (size_t j = 0; j < num_face_edges; ++j) {
					auto& e = edges[face_edges[j]];
					center += e.first_point;
					center += e.second_point;
				}
				center /= float(2 * num_face_edges);
				auto& ref_edge = edges[face_edges[0]];
				vec3 x_axis = normalize((ref_edge.first_point + ref_edge.second_point) * 0.5f - center);
				vec3 y_axis = cross(x_axis, plane.xyz);

				auto get_edge_angle = [&](u8 idx) {
					auto& edge = edges[idx];
					vec3 delta = (edge.first_point + edge.second_point) * 0.5f - center;
					return atan2f(dot(delta, y_axis), dot(delta, x_axis));
				};

				std::sort(std::begin(face_edges), std::begin(face_edges) + num_face_edges, [&](u8 a, u8 b) {
					return get_edge_angle(a) > get_edge_angle(b);
				});

				vec3 points[3];

				for (size_t j = 0; j < 3; ++j) {
					auto& edge = edges[face_edges[2-j]];
					assert(length(edge.first_point - edge.second_point) > 1e-4f);
					vec3& p = (i == edge.first_plane) ? edge.first_point : edge.second_point;
					points[j] = p;
					fprintf(file, "( %g %g %g ) ", p.x, p.y, p.z);
				}

				float align = dot(normalize(points[1] - points[0]), normalize(points[2] - points[1]));
				if (abs(align) >= .99f) {
					int abcd = 0;
				}
				//assert(abs(align) < .99f);

				fprintf(file, "%s %g %g %g %g %g %u %u %u\n",
					map.materials[plane.material].name.c_str(),
					plane.shift.x, plane.shift.y, plane.rotation, plane.scale.x, plane.scale.y,
					plane.content_flags, plane.surface_flags, plane.value
				);
#else
				auto& edge = edges[face_edges[0]];
				vec3 p0 = (i == edge.first_plane) ? edge.first_point : edge.second_point;
				vec3 p1 = (i != edge.first_plane) ? edge.first_point : edge.second_point;
				vec3 p2 = p1 + cross(p1 - p0, plane.xyz);

				fprintf(file, "( %g %g %g ) ( %g %g %g ) ( %g %g %g ) %s %g %g %g %g %g %u %u %u\n",
					p0.x, p0.y, p0.z, p1.x, p1.y, p1.z, p2.x, p2.y, p2.z,
					map.materials[plane.material].name.c_str(),
					plane.shift.x, plane.shift.y, plane.rotation, plane.scale.x, plane.scale.y,
					plane.content_flags, plane.surface_flags, plane.value
				);
#endif
			}

			fprintf(file, "}\n");
		}

		fprintf(file, "}\n");
	}

	return true;
}
