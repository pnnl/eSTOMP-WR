subroutine factor(p,idx,idy,idz)
  implicit none
  integer i,j,p,idx,idy,idz,it
  integer ip,ifac,pmax,prime(1000)
  integer fac(1000)
!
  i = 1
  ip = p
!
!    factor p completely
!    first, find all prime numbers less than or equal to p
!
  pmax = 0
  do i = 2, p
    do j = 1, pmax
      if (mod(i,prime(j)).eq.0) go to 100
    end do
    pmax = pmax + 1
    prime(pmax) = i
    100   continue
  end do
!
!    find all prime factors of p
!
  ifac = 0
  do i = 1, pmax
    200   if (mod(ip,prime(i)).eq.0) then
      ifac = ifac + 1
      fac(ifac) = prime(i)
      ip = ip/prime(i)
      go to 200
    endif
  end do
!
!    determine three factors of p of approximately the
!    same size
!
  idx = 1
  idy = 1
  idz = 1
  do i = ifac, 1, -1
    if (idx.le.idy.and.idx.le.idz) then
      idx = fac(i)*idx
    elseif (idy.le.idx.and.idy.le.idz) then
      idy = fac(i)*idy
    elseif (idz.le.idx.and.idz.le.idy) then
      idz = fac(i)*idz
    endif
  end do
!      it = idy
!      idy = idx
!      idx = it
!
!      it = idx
!      idx = idz
!      idz = it
!
!      it = idy
!      idy = idz
!      idz = it
  return
end
!
subroutine i_proc_to_xyz(p,ix,iy,iz,idx,idy,idz)
  implicit none
  integer p,ix,iy,iz,ip,it
  integer idx,idy,idz
!
  ip = p
!
  it = mod(ip,idx)
  ix = it
  ip = (ip - it)/idx
  it = mod(ip,idy)
  iy = it
  ip = (ip - it)/idy
  it = mod(ip,idz)
  iz = it
!
  return
end
!
subroutine i_xyz_to_proc(p,ix,iy,iz,idx,idy,idz)
  implicit none
  integer p,ix,iy,iz,idx,idy,idz
!
  p = ix + idx*iy + idx*idy*iz
!
  return
end
