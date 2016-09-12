#ifndef Singleton_H
#define Singleton_H

template<typename T>
struct SingletonCreator {
	static void create(T *&theInstance) {
		theInstance = new T;
	}
};
template<typename T, class F = SingletonCreator<T>>
class Singleton {
	public:
		using CapturedType = T;
		using CreatorType = F;
		Singleton() = default;
		~Singleton() = default;
		Singleton(const Singleton &) = delete;
		Singleton(Singleton &&) = delete;
		Singleton(Singleton &) = delete;
		T* get() { create(); return _instance; }
		T& operator *() {create(); return *_instance; }
		T* operator ->() { create(); return _instance; }
		operator T*() { return get(); }

		const T* const_get() const { return _instance; }
		operator const T* () const { return const_get(); }


	private:
		void create() { if (!_instance) { F::create(_instance); } }
		T* _instance = nullptr;
}; // end class Singleton

#endif // end Singleton_H
