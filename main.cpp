
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

template<typename T>
T random_value(std::mt19937& gen, T low, T up) {
    if constexpr (is_complex_v<T>) {
        using R = typename T::value_type;
        if constexpr (std::is_integral_v<R>) {
            std::uniform_int_distribution<R> dist(low.real(), up.real());
            return T(dist(gen), dist(gen));
        }
        else {
            std::uniform_real_distribution<R> dist(low.real(), up.real());
            return T(dist(gen), dist(gen));
        }
    }
    else if constexpr (std::is_integral_v<T>) {
        std::uniform_int_distribution<T> dist(low, up);
        return dist(gen);
    }
    else {
        std::uniform_real_distribution<T> dist(low, up);
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
        _data = new T * [row];
        for (int i = 0; i < row; ++i) {
            _data[i] = new T[column];
        }
    }
    void deallocate() {
        if (_data) {
            for (int i = 0; i < _row; ++i) {
                delete[] _data[i];
            }
            delete[] _data;
        }
    }

public:

    static constexpr double _EPS = 0.0000000000001;

    Matrix(int row, int column, T num) : _row(row), _column(column) {
        if (row <= 0 || column <= 0) throw std::invalid_argument("Bad size for matrix");
        allocate(_row, _column);
        for (int i = 0; i < _row; ++i) {
            for (int j = 0; j < _column; ++j) {
                _data[i][j] = num;
            }
        }
    }

    Matrix(int row, int column, T low, T up) : _row(row), _column(column) {
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

    Matrix(const Matrix& other) : _row(other._row), _column(other._column) {
        allocate(_row, _column);
        for (int i = 0; i < _row; ++i) {
            for (int j = 0; j < _column; ++j) {
                _data[i][j] = other._data[i][j];
            }
        }
    }

    Matrix(Matrix&& other) noexcept
        : _row(other._row), _column(other._column), _data(other._data) {
        other._row = other._column = 0;
        other._data = nullptr;
    }

    Matrix& operator=(const Matrix& other) {
        if (this == &other) return *this;
        deallocate();
        _row = other._row; _column = other._column;
        allocate(_row, _column);
        for (int i = 0; i < _row; ++i)
            for (int j = 0; j < _column; ++j)
                _data[i][j] = other._data[i][j];
        return *this;
    }

    Matrix& operator=(Matrix&& other) noexcept {
        if (this == &other) return *this;
        deallocate();
        _row = other._row; _column = other._column; _data = other._data;
        other._row = other._column = 0; other._data = nullptr;
        return *this;
    }

    ~Matrix() { deallocate(); }

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

    friend std::ostream& operator<<(std::ostream& os, const Matrix& m) {
        for (int i = 0; i < m._row; ++i) {
            for (int j = 0; j < m._column; ++j)
                os << m._data[i][j] << ' ';
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

    std::cout << "Constructors\n";
    Matrix<double> A(3, 3, 1.5);
    std::cout << "A(3,3,1.5):\n" << A;

    Matrix<double> B(3, 3, -5.0, 5.0);
    std::cout << "B(3,3,-5,5) random:\n" << B;

    Matrix<double> C(3, 3, -5.0, 5.0);
    std::cout << "C(3,3,-5,5) random:\n" << C;

    std::cout << "\noperator()\n";
    A(0, 0) = 10.0;
    std::cout << A;


    std::cout << "\ncomparison\n";
    std::cout << "A == B ? " << (B == C) << '\n';
    std::cout << "A != B ? " << (B != C) << '\n';

    std::cout << "\naddition\n";
    Matrix<double> S = B + C;
    std::cout << "A + R:\n" << S;
    

    std::cout << "\nsubstraction\n";
    Matrix<double> D = B - C;
    std::cout << "B - C:\n" << D;

    std::cout << "\nscalar multiplication (commutative)\n";
    Matrix<double> M1 = A * 2.0;
    Matrix<double> M2 = 2.0 * A;
    std::cout << "A * 2:\n" << M1;
    std::cout << "M1 == M2 ? " << (M1 == M2) << '\n';

    std::cout << "\nscalar division\n";
    Matrix<double> V = B / 2.0;
    std::cout << "B / 2:\n" << V;
    

    std::cout << "\nmatrix multiplication\n";
    Matrix<double> P = B * C;
    std::cout << "A * R:\n" << P;

    std::cout << "\n=== trace ===\n";
    std::cout << "trace(B) = " << B.find_tr() << '\n';
    std::cout << "trace(C) = " << C.find_tr() << '\n';

    

    std::cout << "\ncomplex\n";

    using Cmplx  = std::complex<double>;
    Matrix<Cmplx> Z(2, 2, Cmplx{ 1.0, 1.0 });
    std::cout << "Z(2,2,1+1i):\n" << Z;

    Matrix<Cmplx> Zr(2, 2, Cmplx{ -2.0, -2.0 }, Cmplx{ 2.0, 2.0 });
    std::cout << "Zr random:\n" << Zr;

    std::cout << "Z * Zr:\n" << (Z * Zr);
    std::cout << "trace(Z) = " << Z.find_tr() << '\n';

    
    std::cout << "\nTask\n";
    Matrix<double> a(3, 1, 0.0);
    Matrix<double> b(3, 1, 0.0);
    Matrix<double> c(3, 1, 0.0);
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
    std::cout << "Coplanar? " << (is_coplanar(a, b, c) ? "YES" : "NO") << '\n';

    Matrix<double> d(3, 1, 0.0);
    d(0, 0) = 1; d(1, 0) = 0; d(2, 0) = 0;
    std::cout << "Coplanar(a,b,d)? " << (is_coplanar(a, b, d) ? "YES" : "NO") << '\n';

    return 0;
}