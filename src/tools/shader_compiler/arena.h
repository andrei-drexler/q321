#pragma once

struct Arena {
	Arena(size_t block_size = 1 * 1024 * 1024);

	uint8_t*					alloc(size_t size);

	template <typename T, typename... Args>
	T* create(Args&&... args) {
		auto raw = alloc(sizeof(T));
		return ::new(raw) T(std::forward<Args>(args)...);
	}

	template <typename T>
	T* create_n(size_t count) {
		auto raw = alloc(sizeof(T) * count);
		return ::new(raw) T[count]();
	}

private:
	using Block = std::unique_ptr<uint8_t[]>;

	void						grow(size_t size);

	std::vector<Block>			m_blocks;
	size_t						m_block_size = 0;
	uint8_t*					m_cursor = nullptr;
	uint8_t*					m_end = nullptr;
};

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////

Arena::Arena(size_t block_size) :
	m_block_size(block_size)
{ }

void Arena::grow(size_t size) {
	if (size < m_block_size)
		size = m_block_size;
	auto block = std::make_unique<uint8_t[]>(size);
	m_cursor = block.get();
	m_end = block.get() + size;
	m_blocks.push_back(std::move(block));
}

uint8_t* Arena::alloc(size_t size) {
	const size_t alignment = alignof(std::max_align_t);
	size = (size + alignment - 1) & ~(alignment - 1);
	if (size > size_t(m_end - m_cursor))
		grow(size);

	auto ptr = m_cursor;
	m_cursor += size;
	return ptr;
}
