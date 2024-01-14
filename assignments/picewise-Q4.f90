PROGRAM piecewise
  REAL :: H, K, X, Y, FO, FX, FY, FXY, FYX
  PARAMETER (H = 0.00001, K = 0.00001)

  ! Define the function F(X, Y)
  F(X, Y) = (X * Y * (X**2 - Y**2)) / (X**2 + Y**2)

  ! Initialize values
  FO = 0.0
  Y = K

  ! Calculate partial derivative with respect to X (FX) at (H, Y)
  FX = (F(H, Y) - F(0.0, Y)) / H  ! Pass 0.0 as a real constant

  ! Calculate partial derivative with respect to Y (FXY) at (H, Y)
  FXY = (FX - FO) / K

  ! Print the result for FXY
  PRINT *, "FXY =", FXY

  ! Reset values for the next calculation
  X = H
  FY = (F(X, K) - F(X, 0.0)) / K  ! Pass 0.0 as a real constant

  ! Calculate partial derivative with respect to X (FYX) at (H, Y)
  FYX = (FY - FO) / H

  ! Print the result for FYX
  PRINT *, "FYX =", FYX

  ! Compare FXY and FYX
  IF (FXY .NE. FYX) THEN
    PRINT *, "FXY is not equal to FYX"
  END IF

END PROGRAM piecewise
