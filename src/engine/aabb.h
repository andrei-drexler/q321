#pragma once

struct AABB {
	union {
		struct { vec3 mins, maxs; };
		vec3 v[2];
	};

	AABB() = default;

	void clear() {
		mins = vec3(FLT_MAX);
		maxs = vec3(-FLT_MAX);
	}

	void add(const vec3& v) {
		mins = min(mins, v);
		maxs = max(maxs, v);
	}

	void add(const AABB& bounds) {
		mins = min(mins, bounds.mins);
		maxs = max(maxs, bounds.maxs);
	}

	vec3 size() const {
		return maxs - mins;
	}

	bool empty() const {
		return
			maxs.x <= mins.x ||
			maxs.y <= mins.y ||
			maxs.z <= mins.z ;
	}

	vec3 center() const {
		return (maxs + mins) * 0.5f;
	}

	float area() const {
		vec3 dim = maxs - mins;
		return 2.f*(dim.x*dim.y + dim.y*dim.z + dim.z*dim.x);
	}

	bool contains(const vec3& point) const {
		return
			point.x >= mins.x && point.y >= mins.y && point.y >= mins.z &&
			point.x <= maxs.x && point.y <= maxs.y && point.y <= maxs.z;
	}
};

struct Rect {
	union {
		struct { vec2 mins, maxs; };
		vec2 v[2];
	};

	Rect() = default;

	void clear() {
		mins = vec2(FLT_MAX);
		maxs = vec2(-FLT_MAX);
	}

	void add(const vec2& v) {
		mins = min(mins, v);
		maxs = max(maxs, v);
	}

	void add(const Rect& bounds) {
		add(bounds.mins);
		add(bounds.maxs);
	}

	vec2 size() const {
		return maxs - mins;
	}

	float width() const {
		return maxs[0] - mins[0];
	}

	float height() const {
		return maxs[1] - mins[1];
	}

	vec2 center() const {
		return (maxs + mins) * 0.5f;
	}

	float area() const {
		vec2 dim = maxs - mins;
		return dim.x * dim.y;
	}
};
