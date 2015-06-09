!#######################################################################
! 32 bit Fowler/Noll/Vo FNV-1a hash code
! Public Domain
!#######################################################################
! fixed 32 -> 8 bit conversion bug            2013/10/12 AJA
! translated to FORTRAN                       2013/03/11 Andy Allinger
!                                             andy_a@users.sourceforge.net
!
! Revision: 5.1                               2009/06/30 09:13:32  
!#######################################################################
! Fowler/Noll/Vo hash
!
! The basis of this hash algorithm was taken from an idea sent
! as reviewer comments to the IEEE POSIX P1003.2 committee by:
!
!      Phong Vo (http://www.research.att.com/info/kpv/)
!      Glenn Fowler (http://www.research.att.com/~gsf/)
!
! In a subsequent ballot round:
!
!      Landon Curt Noll (http://www.isthe.com/chongo/)
!
! improved on their algorithm.  Some people tried this hash
! and found that it worked rather well.  In an EMail message
! to Landon, they named it the ``Fowler/Noll/Vo'' or FNV hash.
!
! FNV hashes are designed to be fast while maintaining a low
! collision rate. The FNV speed allows one to quickly hash lots
! of data while maintaining a reasonable collision rate.  See:
!
!      http://www.isthe.com/chongo/tech/comp/fnv/index.html
!
! for more details as well as other forms of the FNV hash.
!
!#######################################################################
! Please do not copyright this code.  This code is in the public domain.
!
! LANDON CURT NOLL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
! INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO
! EVENT SHALL LANDON CURT NOLL BE LIABLE FOR ANY SPECIAL, INDIRECT OR
! CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
! USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
! OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
! PERFORMANCE OF THIS SOFTWARE.
!
! By:
!	chongo <Landon Curt Noll> /\oo/\
!      http://www.isthe.com/chongo/
!
! Share and Enjoy!	:-)
!
!#######################################################################
! 32 bit FNV-1 and FNV-1a non-zero initial basis
!
! The FNV-1 initial basis is the FNV-0 hash of the following 32 octets:
!
!              chongo <Landon Curt Noll> /\../\
!
! NOTE: The \'s above are not back-slashing escape characters.
! They are literal ASCII  backslash 0x5c characters.
!
! NOTE: The FNV-1a initial basis is the same value as FNV-1 by definition.
!
!#######################################################################
! perform a 32 bit Fowler/Noll/Vo FNV-1a hash on a buffer
!
! input:
!	BUFFER	- start of buffer to hash
!	LENGTH	- length of buffer in octets [bytes]
!	HASH  	- previous hash value or [standard initial value]
!
! assumption:  integer's are at least 32 bits
!
! NOTE:  To use the recommended 32 bit FNV-1a hash, use the initial value:  
!               HASH = X'811C9DC5' 
!        as the argument on the first call to fnv32
!
! !NOTE:  Only pass the previous hash value if you always hash the same
!         data in the same order (or else you will get different answers!) 
!                    
! returns:
!	32 bit HASH as default integer
!
!#######################################################################
! modified by Albert Huang for modernization
!#######################################################################
module fnv32mod
    use kinds
    
    contains
        subroutine fnv32 (buffer, length, hash)       
            implicit none
            integer length, hash
            integer*1 buffer
            dimension buffer(length)
            
            integer prime ; parameter (prime = 16777619) 
            integer i, j, k
            integer*1 b
        
            !##########################################################
            !                begin
            !##########################################################
            !          fnv-1a hash each octet in the buffer
            do j = 1, length
                b = buffer(j)
                k = 0
                do i = 0, 7           ! copy each bit from b to k
                    if (btest(b, i)) k = ibset(k, i)
                end do
                
                ! xor the bottom with the current octet 
                hash = ieor(hash, k)
                
                ! multiply by the 32 bit fnv magic prime mod 2^32 
                hash = hash * prime
                hash = iand(hash, x'ffffffff')      ! discard > 32 bits
            end do
        end subroutine fnv32
        subroutine fnv32_str (buffer, hash)
            implicit none
            integer(i_kind), intent(inout) :: hash
            character(len=*), intent(in) :: buffer
            
            integer prime ; parameter (prime = 16777619) 
            integer i, j, k
            integer(i_byte) b
        
            !##########################################################
            !                begin
            !##########################################################
            !          fnv-1a hash each octet in the buffer
            do j = 1, len_trim(buffer)
                b = ichar(buffer(j:j))
                k = 0
                do i = 0, 7           ! copy each bit from b to k
                    if (btest(b, i)) k = ibset(k, i)
                end do
                
                ! xor the bottom with the current octet 
                hash = ieor(hash, k)
                
                ! multiply by the 32 bit fnv magic prime mod 2^32 
                hash = hash * prime
                hash = iand(hash, x'ffffffff')      ! discard > 32 bits
            end do
        end subroutine fnv32_str
        
        subroutine fnv16 (buffer, length, hash)
            ! From original website:
            ! For tiny x < 16 bit values, we recommend using a 32 bit FNV-1 hash as follows:
            !
            ! /* NOTE: for 0 < x < 16 ONLY!!! */
            ! #define TINY_MASK(x) (((u_int32_t)1<<(x))-1)
            ! #define FNV1_32_INIT ((u_int32_t)2166136261)
            ! u_int32_t hash;
            ! void *data;
            ! size_t data_len;
            ! 
            ! hash = fnv_32_buf(data, data_len, FNV1_32_INIT);
            ! hash = (((hash>>x) ^ hash) & TINY_MASK(x));
            
            implicit none
            integer length, hash
            integer*1 buffer
            dimension buffer(length)
            
            integer prime ; parameter (prime = 16777619)
            integer bits ; parameter (bits = 16)
            integer i, j, k
            integer*1 b
        
            !##########################################################
            !                begin
            !##########################################################
            !          fnv-1a hash each octet in the buffer
            do j = 1, length
                b = buffer(j)
                k = 0
                do i = 0, 7           ! copy each bit from b to k
                    if (btest(b, i)) k = ibset(k, i)
                end do
                
                ! xor the bottom with the current octet 
                hash = ieor(hash, k)
                
                ! multiply by the 32 bit fnv magic prime mod 2^32 
                hash = hash * prime
                hash = iand(hash, x'ffffffff')      ! discard > 32 bits
            end do
            
            ! Recall that to shrink this down, in C we need:
            ! hash = (((hash>>x) ^ hash) & TINY_MASK(x));
            ! 
            ! In fortran, we need a few funcs:
            ! ieor(x, y) - xor
            ! ishft(x, shift) - >> if shift is neg, << if pos
            hash = iand(ieor(ishft(hash, -bits), hash), ishft(1, bits) - 1)
        end subroutine fnv16
        
        subroutine fnv16_str (buffer, hash)
            ! From original website:
            ! For tiny x < 16 bit values, we recommend using a 32 bit FNV-1 hash as follows:
            !
            ! /* NOTE: for 0 < x < 16 ONLY!!! */
            ! #define TINY_MASK(x) (((u_int32_t)1<<(x))-1)
            ! #define FNV1_32_INIT ((u_int32_t)2166136261)
            ! u_int32_t hash;
            ! void *data;
            ! size_t data_len;
            ! 
            ! hash = fnv_32_buf(data, data_len, FNV1_32_INIT);
            ! hash = (((hash>>x) ^ hash) & TINY_MASK(x));
    
            implicit none
            integer(i_kind), intent(inout) :: hash
            character(len=*), intent(in) :: buffer
            
            integer prime ; parameter (prime = 16777619) 
            integer bits ; parameter (bits = 16)
            integer i, j, k
            integer(i_byte) b
        
            !##########################################################
            !                begin
            !##########################################################
            !          fnv-1a hash each octet in the buffer
            do j = 1, len_trim(buffer)
                b = ichar(buffer(j:j))
                k = 0
                do i = 0, 7           ! copy each bit from b to k
                    if (btest(b, i)) k = ibset(k, i)
                end do
                
                ! xor the bottom with the current octet 
                hash = ieor(hash, k)
                
                ! multiply by the 32 bit fnv magic prime mod 2^32 
                hash = hash * prime
                hash = iand(hash, x'ffffffff')      ! discard > 32 bits
            end do
            
            ! Recall that to shrink this down, in C we need:
            ! hash = (((hash>>x) ^ hash) & TINY_MASK(x));
            ! 
            ! In fortran, we need a few funcs:
            ! ieor(x, y) - xor
            ! ishft(x, shift) - >> if shift is neg, << if pos
            hash = iand(ieor(ishft(hash, -bits), hash), ishft(1, bits) - 1)
        end subroutine fnv16_str
      !############## of file fnv32.f ##############################
end module fnv32mod

