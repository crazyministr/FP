-- Малашенков Антон, гр. А3401

module IntegrCos where

-- Написать функцию integrCos :: Double -> Double,
-- которая вычисляет приближенное значение интеграла
-- функции x cos x на промежутке [0, pi/2] методом прямоугольников.
-- Функция должна производить вычисления за несколько шагов,
-- удваивая на каждом шаге число элементарных отрезков,
-- на которые делится промежуток интегрирования.
-- Вычисления следует прекратить, когда результаты, полученные на двух очередных шагах,
-- станут отличаться друг от друга меньше, чем на величину, заданную аргументом функции.


integrCos :: Double -> Double
integrCos eps = integrCos' eps (calcIntegral 0 (pi / 2) 0 0) 1

integrCos' :: Double -> Double -> Integer -> Double
integrCos' eps prevResult totalSteps
    | abs(prevResult - currentResult) < eps = currentResult
    | otherwise = integrCos' eps currentResult (totalSteps + 1)
      where currentResult = calcIntegral 0 (pi / 2) 0 totalSteps

calcIntegral :: Double -> Double -> Integer -> Integer -> Double
calcIntegral l r currentStep totalSteps
    | currentStep == totalSteps = (r - l) * (l + (r - l) / 2) * cos(l + (r - l) / 2)
    | otherwise = calcIntegral l (l + (r - l) / 2) (currentStep + 1) totalSteps +
                  calcIntegral (l + (r - l) / 2) r (currentStep + 1) totalSteps

