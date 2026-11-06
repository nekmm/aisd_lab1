
#include <iostream>
#include <random>
#include <complex>
#include <stdexcept>
#include <type_traits>
#include <cmath>
#include <utility>


template<typename U> struct is_complex : std::false_type {};
template<typename U> struct is_complex<std::complex<U>> : std::true_type {};
template<typename U> inline constexpr bool is_complex_v = is_complex<U>::value;

template<typename T>
bool equals(const T& a, const T& b, double eps) {
    if constexpr (is_complex_v<T>) {
        return std::abs(a - b) <= eps;
    }
    else if constexpr (std::is_integral_v<T>) {
        return a == b;
    }
    else {
        return std::abs(a - b) <= static_cast<T>(eps);
    }
}

template<typename R>
auto make_dist(R low, R up) {
    if constexpr (std::is_integral_v<R>) {
        return std::uniform_int_distribution<R>(low, up);
    }
    else {
        return std::uniform_real_distribution<R>(low, up);
    }
}

template<typename T>
T random_value(std::mt19937& gen, T low, T up) {
    if constexpr (is_complex_v<T>) {
        using R = typename T::value_type;
        auto dist = make_dist(low.real(), up.real());
        R re = dist(gen);
        R im = dist(gen);
        return T(re, im);
    }
    else {
        auto dist = make_dist(low, up);
        return dist(gen);
    }
}

template<typename T>
class Matrix {
private:
    int _row = 0;
    int _column = 0;
    T** _data = nullptr;

    void allocate(int row, int column) {
        _data = new T*[row];
        for (int i = 0; i < row; ++i) {
            _data[i] = new T[column];
        }
    }
    void clear() {
        if (_data) {
            for (int i = 0; i < _row; ++i) {
                delete[] _data[i];
            }
            delete[] _data;
        }
    }

public:

    static constexpr double _EPS = 0.0000000000001;

    Matrix(int row, int column, T num) : _row(row), _column(column) { //заполнение матрицы 1 значением
	
        if (row <= 0 || column <= 0) throw std::invalid_argument("Bad size for matrix");
        allocate(_row, _column);
        for (int i = 0; i < _row; ++i) {
            for (int j = 0; j < _column; ++j) {
                _data[i][j] = num;
            }
        }
    }

    Matrix(int row, int column, T low, T up) : _row(row), _column(column) { //заполнение матрицы рандомными значениями
        if (row <= 0 || column <= 0) throw std::invalid_argument("Bad size for matrix");
		
        std::random_device rd;
        std::mt19937 gen(rd());
		
        allocate(_row, _column);
		
        for (int i = 0; i < _row; ++i) {
            for (int j = 0; j < _column; ++j) {
                _data[i][j] = random_value<T>(gen, low, up);
            }
        }
    }

    Matrix(const Matrix& other) : _row(other._row), _column(other._column) { //конструктор копирования
        allocate(_row, _column);
        for (int i = 0; i < _row; ++i) {
            for (int j = 0; j < _column; ++j) {
                _data[i][j] = other._data[i][j];
            }
        }
    }

    Matrix(Matrix&& other) noexcept : _row(other._row), _column(other._column), _data(other._data) { //конструктор перемещения
        other._row = other._column = 0;
        other._data = nullptr;
    }

    Matrix& operator=(const Matrix& other) { //оператор присваивания копированием
        if (this == &other) return *this;
        clear();
        _row = other._row; 
        _column = other._column;
        allocate(_row, _column);
        for (int i = 0; i < _row; ++i) {
            for (int j = 0; j < _column; ++j) {
                _data[i][j] = other._data[i][j];
            }
        }
       return *this;
    }

    Matrix& operator=(Matrix&& other) noexcept { //оператор присваивания перемещением
        if (this == &other) return *this;
        clear();
        _row = other._row; 
        _column = other._column; 
        _data = other._data;
        other._row = other._column = 0; 
        other._data = nullptr;
        return *this;
    }

    ~Matrix() { clear(); } //деструктор
	
	////////////////////////////////////

    bool operator==(const Matrix& other) const {
        if (_row != other._row || _column != other._column)
            return false;
        for (int i = 0; i < _row; ++i)
            for (int j = 0; j < _column; ++j)
                if (!equals(_data[i][j], other._data[i][j], _EPS))
                    return false;
        return true;
    }

    bool operator!=(const Matrix& other) const {
        return !(*this == other);
    }

    T& operator()(int row, int column) {
        if (row < 0 || row >= _row || column < 0 || column >= _column) {
            throw std::out_of_range("Wrong index");
        }
        return _data[row][column];
    }

    const T& operator()(int row, int column) const {
        if (row < 0 || row >= _row || column < 0 || column >= _column)
            throw std::out_of_range("Wrong index");
        return _data[row][column];
    }

    int row() const { return _row; }
    int col() const { return _column; }

    Matrix& operator+=(const Matrix& other) {
        if (_row != other._row || _column != other._column) {
            throw std::invalid_argument("Wrong matrix");
        }
        for (int i = 0; i < _row; ++i) {
            for (int j = 0; j < _column; ++j) {
                _data[i][j] += other._data[i][j];
            }
        }
        return *this;
    }

    Matrix operator+(const Matrix& other) const {
        Matrix result = *this;
        result += other;
        return result;
    }

    Matrix& operator-=(const Matrix& other) {
        if (_row != other._row || _column != other._column) {
            throw std::invalid_argument("Wrong matrix for sum");
        }
        for (int i = 0; i < _row; ++i) {
            for (int j = 0; j < _column; ++j) {
                _data[i][j] -= other._data[i][j];
            }
        }
        return *this;
    }

    Matrix operator-(const Matrix& other) const {
        Matrix result = *this;
        result -= other;
        return result;
    }

    Matrix& operator*=(T num) {
        for (int i = 0; i < _row; ++i) {
            for (int j = 0; j < _column; ++j) {
                _data[i][j] *= num;
            }
        }
        return *this;
    }

    Matrix operator*(T num) const {
        Matrix result = *this;
        result *= num;
        return result;
    }

    friend Matrix operator*(T num, const Matrix<T>& other) {
        return other * num;
    }

    Matrix& operator/=(T num) {
        if constexpr (is_complex_v<T>) {
            if (std::abs(num) <= _EPS) throw std::invalid_argument("Matrix: division by zero");
        }
        else if constexpr (std::is_integral_v<T>) {
            if (num == 0) throw std::invalid_argument("Matrix: division by zero");
        }
        else {
            if (std::abs(num) <= static_cast<T>(_EPS)) throw std::invalid_argument("Matrix: division by zero");
        }
        for (int i = 0; i < _row; ++i)
            for (int j = 0; j < _column; ++j)
                _data[i][j] /= num;
        return *this;
    }

    Matrix operator/(T num) const {
        Matrix result = *this;
        result /= num;
        return result;
    }

    T find_tr() const {
        T num_of_tr = T{};
        if (_row != _column) {
            throw std::invalid_argument("Wrong matrix: should be square matrix");
        }
        for (int i = 0; i < _row; ++i) {
            num_of_tr += _data[i][i];
        }
        return num_of_tr;
    }

    Matrix operator*(const Matrix& other) const {
        if (_column != other._row) {
            throw std::invalid_argument("Wrong matrix for multiply");
        }
        Matrix result(_row, other._column, T{});
        for (int i = 0; i < _row; ++i) {
            for (int j = 0; j < other._column; ++j) {
                T sum = T{};
                for (int k = 0; k < _column; ++k) {
                    sum += _data[i][k] * other._data[k][j];
                }
                result._data[i][j] = sum;
            }
        }
        return result;
    }

    friend std::ostream& operator<<(std::ostream& os, const Matrix<T>& m) {
        for (int i = 0; i < m._row; ++i) {
            for (int j = 0; j < m._column; ++j) {
                os << m._data[i][j] << ' ';
            }
            os << '\n';
        }
        return os;
    }

};

template<typename T>
bool is_coplanar(const Matrix<T>& a, const Matrix<T>& b, const Matrix<T>& c) {

    if (a.row() != 3 || a.col() != 1 || b.row() != 3 || b.col() != 1 || c.row() != 3 || c.col() != 1) {
        throw std::invalid_argument("Expected 3x1 vectors");
    }
    T det = a(0, 0) * (b(1, 0) * c(2, 0) - b(2, 0) * c(1, 0)) - a(1, 0) * (b(0, 0) * c(2, 0) - b(2, 0) * c(0, 0)) + a(2, 0) * (b(0, 0) * c(1, 0) - b(1, 0) * c(0, 0));
    return equals(det, T{}, Matrix<T>::_EPS);

}

int main() {

    std::cout << "\n1. Constructors\n\n";
    Matrix<double> A(3, 3, 1.5);
    std::cout << "A(3,3, 1.5):\n" << A;

    Matrix<double> B(3, 3, -5.0, 5.0);
    std::cout << "B(3,3, random[-5,5]):\n" << B;

    Matrix<double> C(3, 3, -5.0, 5.0);
    std::cout << "C(3,3, random[-5,5]):\n" << C;


    std::cout << "\n2. operator()\n\n";

    std::cout << "A(0,0) before = " << A(0, 0) << '\n';
    A(0, 0) = 10.0;
    std::cout << "A(0,0) after  = " << A(0, 0) << "\n\n";

    std::cout << "A:\n" << A;


    std::cout << "\n3. Comparison\n\n";
    std::cout << "B == C ? " << (B == C) << '\n';
    std::cout << "B != C ? " << (B != C) << '\n';
    std::cout << "B == B ? " << (B == B) << '\n';


    std::cout << "\n4. + / - \n\n";
    std::cout << "B + C:\n" << (B + C);
    std::cout << "B - C:\n" << (B - C);


    std::cout << "\n5. Scalar multiplication\n\n";
    Matrix<double> M1 = A * 2.0;
    Matrix<double> M2 = 2.0 * A;
    std::cout << "A * 2:\n" << M1;
    std::cout << "A*2 == 2*A ? " << (M1 == M2) << '\n';


    std::cout << "\n6. Scalar division\n\n";
    std::cout << "B / 2:\n" << (B / 2.0);


    std::cout << "\n7. Matrix multiplication\n\n";
    std::cout << "B * C:\n" << (B * C);

    std::cout << "\n8. Trace\n\n";
    std::cout << "trace(B) = " << B.find_tr() << '\n';
    std::cout << "trace(C) = " << C.find_tr() << '\n';

    //Комплексные
    std::cout << "\n9. std::complex<double>\n\n";
    using Cmplx = std::complex<double>;
    Matrix<Cmplx> Z(2, 2, Cmplx{ 1.0, 1.0 });
    std::cout << "Z(2,2, 1+1i):\n" << Z;

    Matrix<Cmplx> Zr(2, 2, Cmplx{ -2.0, -2.0 }, Cmplx{ 2.0, 2.0 });
    std::cout << "Zr(2,2, random):\n" << Zr;

    std::cout << "Z * Zr:\n" << (Z * Zr);
    std::cout << "trace(Z) = " << Z.find_tr() << '\n';


    std::cout << "\n10. Matrix<int>\n\n";
    Matrix<int> I(3, 3, -3, 3);
    std::cout << "I(3,3, random[-3,3]):\n" << I;
    std::cout << "trace(I) = " << I.find_tr() << '\n';
    std::cout << "I + I:\n" << (I + I);
    std::cout << "I * I:\n" << (I * I);


    std::cout << "\n11. Matrix<float>\n\n";
    Matrix<float> F(2, 3, -1.0f, 1.0f);
    std::cout << "F(2,3, random[-1,1]):\n" << F;
    std::cout << "F * 2:\n" << (F * 2.0f);
    std::cout << "F / 2:\n" << (F / 2.0f);


    std::cout << "\n12. std::complex<float>\n\n";
    using CmplxF = std::complex<float>;
    Matrix<CmplxF> Zf(2, 2, CmplxF{ 1.0f, -1.0f });
    std::cout << "Zf(2,2, 1-1i):\n" << Zf;
    std::cout << "Zf * 2:\n" << (Zf * 2.0f);


    std::cout << "\n13. Task: coplanarity (deterministic, coplanar)\n\n";
    Matrix<double> a(3, 1, 0.0), b(3, 1, 0.0), c(3, 1, 0.0);
    a(0, 0) = 1; 
    a(1, 0) = 2; 
    a(2, 0) = 3;

    b(0, 0) = 4; 
    b(1, 0) = 5; 
    b(2, 0) = 6;

    c(0, 0) = 7; 
    c(1, 0) = 8; 
    c(2, 0) = 9;

    std::cout << "a:\n" << a << "b:\n" << b << "c:\n" << c;
    std::cout << "a, b, c coplanar? " << (is_coplanar(a, b, c) ? "Y" : "N") << '\n';

    std::cout << "\n14. Task: coplanarity (non-coplanar)\n\n";
    Matrix<double> d(3, 1, 0.0);

    d(0, 0) = 1; 
    d(1, 0) = 0; 
    d(2, 0) = 0;

    std::cout << "d:\n" << d;
    std::cout << "a, b, d coplanar? " << (is_coplanar(a, b, d) ? "Y" : "N") << '\n';


    std::cout << "\n15. Task: coplanarity (random)\n\n";
    Matrix<double> ra(3, 1, -3.0, 3.0);
    Matrix<double> rb(3, 1, -3.0, 3.0);
    Matrix<double> rc(3, 1, -3.0, 3.0);
    std::cout << "ra:\n" << ra << "rb:\n" << rb << "rc:\n" << rc;
    std::cout << "ra, rb, rc coplanar? " << (is_coplanar(ra, rb, rc) ? "Y" : "N") << '\n';

    Matrix<double> rc2 = ra + rb;
    std::cout << "rc2 = ra + rb:\n" << rc2;
    std::cout << "ra, rb, rc2 coplanar? " << (is_coplanar(ra, rb, rc2) ? "Y" : "N") << '\n';

    

}