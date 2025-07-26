module test_suite1
use testdrive, only : new_unittest, unittest_type, error_type, check
implicit none
  private

  public :: collect_suite1

contains

!> Collect all exported unit tests
subroutine collect_suite1(testsuite)
  !> Collection of tests
  type(unittest_type), allocatable, intent(out) :: testsuite(:)

  testsuite = [ &
    new_unittest("mp_negz_arr", &
      test_mp_negz_arr), &
    new_unittest("mp_mulz_zarr", &
      test_mp_mulz_zarr) &
    ]

end subroutine collect_suite1
!
!{{{ mp_negz_arr
subroutine test_mp_negz_arr(error)
use mpmodule
implicit none
!
  type(error_type), allocatable, intent(out) :: error
!
  integer :: i
  type(mp_real) :: val1, val2
  type(mp_complex) :: zval1, zval2, zval3, zval4, zval5
  type(mp_complex) , dimension(:) , allocatable :: arr, m_arr
!
  val1 = '0.0'; val2 = '1.0'
  zval1 = mpcmplx(val1, val2)
!
  val1 = '0.0'; val2 = '1.0'
  zval2 = mpcmplx(val2, val1)
!
  val1 = '1.0'; val2 = '2.0'
  zval3 = mpcmplx(val1, val2)
!
  val1 = '2.0'; val2 = '3.0'
  zval4 = mpcmplx(val1, val2)
!
  val1 = '3.0'; val2 = '4.0'
  zval5 = mpcmplx(val1, val2)
!
  allocate( &
    arr(1:5), &
    m_arr(1:5))
!
  arr = (/zval1, zval2, zval3, zval4, zval5/)
!
  m_arr = -arr
!
  do i=lbound(arr, 1), ubound(arr, 1)
    call check(error, m_arr(i).eq.-arr(i), .true.)
    if (allocated(error)) return
  end do
!
  deallocate(arr, m_arr)
!
  allocate( &
    arr(-2:2), &
    m_arr(-2:2))
!
  arr = (/zval1, zval2, zval3, zval4, zval5/)
!
  m_arr = -arr
!
  do i=lbound(arr, 1), ubound(arr, 1)
    call check(error, m_arr(i).eq.-arr(i), .true.)
    if (allocated(error)) return
  end do
!
end subroutine test_mp_negz_arr
!}}}

!{{{ mp_mulz_zarr
subroutine test_mp_mulz_zarr(error)
use mpmodule
implicit none
!
  type(error_type), allocatable, intent(out) :: error
!
  integer :: i, j
!
  type(mp_real)    :: val1, val2
  type(mp_complex) :: zval
!
  integer , dimension(3) , parameter :: lbound_arr = (/1, 0, -2/)
  integer , dimension(3) , parameter :: ubound_arr = (/5, 4, +2/)
  type(mp_complex) , dimension(:) , allocatable :: arr1, arr2
!
  val1 = '1.5'
  val2 = '2.5'
!
  zval = mpcmplx(val1, val2)
!
  do i=1, size(lbound_arr, 1)
    allocate( &
      arr1(lbound_arr(i) : ubound_arr(i)), &
      arr2(lbound_arr(i) : ubound_arr(i))  &
    )
!
    do j=lbound_arr(i), ubound_arr(i)
      arr1(j) = mpcmplx(cmplx(j, 0, kind=kind(1d0)))
    end do
!
    arr2 = zval*arr1
!
    do j=lbound_arr(i), ubound_arr(i)
      call check(error, arr2(j).eq.zval*arr1(j), .true.)
      if (allocated(error)) return
    end do
!
    deallocate(arr1, arr2)
  end do
!
end subroutine test_mp_mulz_zarr
!}}}
!
end module test_suite1
