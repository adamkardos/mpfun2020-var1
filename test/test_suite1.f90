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
      test_mp_negz_arr) &
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
!
end module test_suite1
