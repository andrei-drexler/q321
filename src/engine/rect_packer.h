#pragma once

struct alignas(u32) RectPacker {
	using Dimension				= u16;
	using Index					= u16;

	struct Rectangle {
		Dimension				min[2];
		Dimension				max[2];

		Dimension				GetWidth() const { return max[0] - min[0]; }
		Dimension				GetHeight() const { return max[1] - min[1]; }
	};
	static constexpr Rectangle*	Full = nullptr;

	void						Init(Dimension width, Dimension height);

	const Rectangle*			Add(Dimension width, Dimension height);
	Index						GetIndex(const Rectangle* rectangle) const { return rectangle - &m_tiles[0]; }
	const Rectangle&			GetTile(Index index) const	{ return m_tiles[index]; }
	Index						GetNumTiles() const			{ return m_numTiles; }

	Dimension					GetWidth() const			{ return m_width; }
	Dimension					GetHeight() const			{ return m_height; }

private:
	struct Node {
		using Field				= Dimension;

		Field					flags;
		Field					data;

		static const Field		Empty = -1;
		static const Field		AxisBits = 2;
		static const Field		AxisMask = (1 << AxisBits) - 1;
		static const Field		LeafTag = AxisMask;
		static const Field		MaxIndex = Field(-1) >> AxisBits;

		Field					GetAxis() const				{ return flags & AxisMask; }
		bool					IsLeaf() const				{ return (flags & AxisMask) == LeafTag; }
		Field					GetNodeSplitPos() const		{ return data; }
		Field					GetNodeFirstChild() const	{ return flags >> AxisBits; }
		Field					IsLeafEmpty() const			{ return data == Empty; }
		Field					GetLeafImageIndex() const	{ return data; }

		void					MakeLeaf(Field data = Empty)								{ this->flags = LeafTag; this->data = data; }
		void					MakeInternal(bool axis, Field split_pos, Field child)		{ this->flags = Field((child << AxisBits) | Field(axis)); this->data = split_pos; }
	};

	bool						DoAdd(u16 node_index, Rectangle node_rect, const Dimension wanted[2]);

	enum {
		MaxTiles				= 8192,
		MaxNodes				= 16384,
	};

	Dimension					m_width;
	Dimension					m_height;
	u16							m_numNodes;
	u16							m_numTiles;
	Array<Node, MaxNodes>		m_nodes;
	Array<Rectangle, MaxTiles>	m_tiles;
};

////////////////////////////////////////////////////////////////

void RectPacker::Init(Dimension width, Dimension height) {
	m_width = width;
	m_height = height;
	m_numNodes = 1;
	m_numTiles = 0;
	m_nodes[0].MakeLeaf();
}

FORCEINLINE bool RectPacker::DoAdd(u16 node_index, Rectangle node_rect, const Dimension wanted[2]) {
	struct StackEntry {
		u16 node_index;
		Rectangle node_rect;
	};

	const u16 MaxDepth = 128;
	StackEntry stack[MaxDepth];
	u16 stack_top = 0;

	goto beginning;

pop:
	if (stack_top == 0) {
		return false;
	} else {
		auto& top = stack[--stack_top];
		node_index = top.node_index;
		node_rect = top.node_rect;
	}

beginning:
	assert(node_index < m_numNodes);

	auto& node = m_nodes[node_index];
	if (node.IsLeaf()) {
		if (!node.IsLeafEmpty())
			goto pop;

		u32 width = node_rect.GetWidth();
		u32 height = node_rect.GetHeight();
		u32 delta_x = width - wanted[0];
		u32 delta_y = height - wanted[1];
		i32 delta_xy = delta_x | delta_y;

		if (delta_xy < 0) // width < wanted[0] || height < wanted[1]
			goto pop;

		if (delta_xy == 0) { // width == wanted[0] && height == wanted[1]
			m_nodes[node_index].data = m_numTiles;
			m_tiles[m_numTiles++] = node_rect;
			assert(m_numTiles <= size(m_tiles));
			return true;
		}

		bool axis = delta_y > delta_x;
		Node::Field split_pos = node_rect.min[axis] + wanted[axis];

		auto first_child = (Node::Field) m_numNodes;
		m_nodes[first_child + 0].MakeLeaf();
		m_nodes[first_child + 1].MakeLeaf();
		m_numNodes += 2;
		assert(m_numNodes <= size(m_nodes));

		node.MakeInternal(axis, split_pos, first_child);
	}

	auto axis = node.GetAxis();
	auto split_pos = node.GetNodeSplitPos();
	auto first_child = node.GetNodeFirstChild();

	assert(stack_top < MaxDepth);
	auto& push = stack[stack_top++];
	push.node_index = first_child + 1;
	MemCopy(&push.node_rect, &node_rect);
	push.node_rect.min[axis] = split_pos;

	node_rect.max[axis] = split_pos;
	node_index = first_child;

	goto beginning;
}

NOINLINE const RectPacker::Rectangle* RectPacker::Add(Dimension width, Dimension height) {
	assert(m_numTiles < MaxTiles);

	Dimension wanted[2] = {width, height};
	Rectangle full{{0, 0}, {m_width, m_height}};

	if (!DoAdd(0, full, wanted))
		Sys::Fatal(Error::Atlas);

	return &m_tiles[m_numTiles - 1];
}
