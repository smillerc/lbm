module mod_xdmf

  use iso_fortran_env, only: real32
  use mod_functional, only: operator(.reverse.)
  use mod_kinds, only: ik, rk
  use mod_io, only: contour_writer_t
  ! use mod_grid, only : grid_t, grid_2d_t, grid_3d_t
  use hdf5_interface, only: hdf5_file

  implicit none

  private
  public :: xdmf_contour_t

  type, extends(contour_writer_t) :: xdmf_contour_t
    private
    type(hdf5_file) :: h5_file

    character(:), allocatable :: xml_filename
    character(:), allocatable :: h5_filename

    character(:), allocatable :: dimension_str
    character(:), allocatable :: topology_type
    character(:), allocatable :: time_str
    integer(ik) :: xml_file_id
  contains
    procedure, public :: initialize => init_xdmf
    procedure, public :: finalize => final_xdmf
    procedure, public :: add_header => write_xdmf_header
    procedure, private :: add_int_1d => xdmf_add_int_1d
    procedure, private :: add_int_2d => xdmf_add_int_2d
    procedure, private :: add_int_3d => xdmf_add_int_3d
    procedure, private :: add_real64_1d => xdmf_add_real64_1d
    procedure, private :: add_real64_2d => xdmf_add_real64_2d
    procedure, private :: add_real64_3d => xdmf_add_real64_3d
    procedure, private :: init_xml
    procedure, private :: final_xml
    procedure, private :: write_xml_header
    procedure, private :: write_xml_grid
    procedure, private :: add_xml_variable_1d
    procedure, private :: add_xml_variable_2d
    procedure, private :: add_xml_variable_3d
  end type xdmf_contour_t

contains

  subroutine init_xml(self)
    class(xdmf_contour_t), intent(inout) :: self
    character(50) :: char_buffer
    integer(ik) :: ios

    open(newunit=self%xml_file_id, file=self%xml_filename, &
         iostat=ios, status="replace", action="write")

    if(ios /= 0) then
      print *, "Error opening xdmf file: '"//self%xml_filename//"'"
      error stop "File I/O Problem"
    end if

    select case(self%ndim)
    case(2)
      self%topology_type = '2DRectMesh'

      char_buffer = ''
      write(char_buffer, '(2(i0, 1x))') self%shape
      self%dimension_str = trim(char_buffer)
    case(3)
      self%topology_type = '3DRectMesh'

      char_buffer = ''
      write(char_buffer, '(3(i0, 1x))') self%shape
      self%dimension_str = trim(char_buffer)
    case default
      error stop 'Error: mod_xdmf only supports 2D and 3D grids'
    end select

    char_buffer = ''
    write(char_buffer, '(f0.4)') self%time
    self%time_str = trim(char_buffer)
  end subroutine init_xml

  subroutine final_xml(self)
    class(xdmf_contour_t), intent(inout) :: self

    write(self%xml_file_id, '(2(2x),a)') '</Grid>'
    write(self%xml_file_id, '(1(2x),a)') '</Domain>'
    write(self%xml_file_id, '(a)') '</Xdmf>'
    close(self%xml_file_id)
  end subroutine final_xml

  subroutine write_xml_header(self)
    class(xdmf_contour_t), intent(inout) :: self

    write(self%xml_file_id, '(a)') '<?xml version="1.0"?>'
    write(self%xml_file_id, '(a)') '<Xdmf version="2.2">'
    write(self%xml_file_id, '(1(2x),a)') '<Domain>'
    write(self%xml_file_id, '(2(2x),a)') '<Grid GridType="Uniform" Name="grid">'
    write(self%xml_file_id, '(3(2x),a)') '<Time Value="'//self%time_str//'"/>'
    write(self%xml_file_id, '(3(2x),a,3(i0,1x),a)') '<Topology NumberOfElements="', &
      .reverse.self%shape, '" TopologyType="'//self%topology_type//'"/>'
  end subroutine write_xml_header

  subroutine write_xml_grid(self)
    class(xdmf_contour_t), intent(inout) :: self

    if(self%ndim == 2) then
      write(self%xml_file_id, '(3(2x),a)') '<Geometry GeometryType="VXVYVZ">'

      write(self%xml_file_id, '(4(2x),a,i0,a)') '<DataItem Dimensions="', self%shape(1), '"'// &
        ' Format="HDF" NumberType="Float"'// &
        ' Precision="4">'//self%h5_filename//':/x</DataItem>'

      write(self%xml_file_id, '(4(2x),a,i0,a)') '<DataItem Dimensions="', self%shape(2), '"'// &
        ' Format="HDF" NumberType="Float"'// &
        ' Precision="4">'//self%h5_filename//':/y</DataItem>'

      write(self%xml_file_id, '(3(2x),a)') '</Geometry>'

    else if(self%ndim == 3) then
      write(self%xml_file_id, '(3(2x),a)') '<Geometry GeometryType="VXVYVZ">'

      write(self%xml_file_id, '(4(2x),a,i0,a)') '<DataItem Dimensions="', self%shape(1), '"'// &
        ' Format="HDF" NumberType="Float"'// &
        ' Precision="4">'//self%h5_filename//':/x</DataItem>'

      write(self%xml_file_id, '(4(2x),a,i0,a)') '<DataItem Dimensions="', self%shape(2), '"'// &
        ' Format="HDF" NumberType="Float"'// &
        ' Precision="4">'//self%h5_filename//':/y</DataItem>'

      write(self%xml_file_id, '(4(2x),a,i0,a)') '<DataItem Dimensions="', self%shape(3), '"'// &
        ' Format="HDF" NumberType="Float"'// &
        ' Precision="4">'//self%h5_filename//':/z</DataItem>'

      write(self%xml_file_id, '(3(2x),a)') '</Geometry>'
    end if

  end subroutine write_xml_grid

  subroutine add_xml_variable_1d(self, name)

    class(xdmf_contour_t), intent(inout) :: self
    character(*), intent(in) :: name

    write(self%xml_file_id, '(3(2x),a)') '<Attribute AttributeType="Scalar" Center="Node" Name="'//name//'">'
    write(self%xml_file_id, '(4(2x),a,1(i0,1x),a)') '<DataItem Dimensions="', 1, '" Format="HDF" '// &
      'NumberType="Float" Precision="4">'//self%h5_filename//':/'//name//'</DataItem>'
    write(self%xml_file_id, '(3(2x),a)') '</Attribute>'

  end subroutine add_xml_variable_1d

  subroutine add_xml_variable_2d(self, name, data_shape)

    class(xdmf_contour_t), intent(inout) :: self
    character(*), intent(in) :: name
    integer(ik), dimension(2), intent(in) :: data_shape

    write(self%xml_file_id, '(3(2x),a)') '<Attribute AttributeType="Scalar" Center="Node" Name="'//name//'">'
    write(self%xml_file_id, '(4(2x),a,2(i0,1x),a)') '<DataItem Dimensions="', .reverse.data_shape, '" Format="HDF" '// &
      'NumberType="Float" Precision="4">'//self%h5_filename//':/'//name//'</DataItem>'
    write(self%xml_file_id, '(3(2x),a)') '</Attribute>'

  end subroutine add_xml_variable_2d

  subroutine add_xml_variable_3d(self, name, data_shape)

    class(xdmf_contour_t), intent(inout) :: self
    character(*), intent(in) :: name
    integer(ik), dimension(3), intent(in) :: data_shape

    write(self%xml_file_id, '(3(2x),a)') '<Attribute AttributeType="Scalar" Center="Node" Name="'//name//'">'
    write(self%xml_file_id, '(4(2x),a,3(i0,1x),a)') '<DataItem Dimensions="', .reverse.data_shape, '" Format="HDF" '// &
      'NumberType="Float" Precision="4">'//self%h5_filename//':/'//name//'</DataItem>'
    write(self%xml_file_id, '(3(2x),a)') '</Attribute>'

  end subroutine add_xml_variable_3d

  subroutine init_xdmf(self, timestep, time, nx_ny_nz, ndim)
    !! Initialize the xdmf contour writer

    class(xdmf_contour_t), intent(inout) :: self
    real(rk), intent(in) :: time
    integer(ik), intent(in) :: timestep
    integer(ik), intent(in) :: nx_ny_nz(3)
    integer(ik), intent(in) :: ndim

    ! Locals
    character(50) :: char_buff

    self%format = 'xdmf'
    self%time = time
    self%ndim = ndim
    self%timestep = timestep

    write(char_buff, '(a,i0.7)') self%filename_prefix, self%timestep
    self%filename = trim(char_buff)
    self%h5_filename = self%filename//'.h5'
    self%xml_filename = self%filename//'.xdmf'

    allocate(self%shape(ndim))

    if(this_image() /= 1) then
      error stop "Error, init_xdmf in "//__FILE__//" should only be called by image 1"
    endif

    call self%init_xml()

    call self%h5_file%initialize(filename=self%h5_filename, &
                                 status='new', action='w', comp_lvl=6)

  end subroutine init_xdmf

  subroutine final_xdmf(self)
    class(xdmf_contour_t), intent(inout) :: self

    call self%final_xml()
    call self%h5_file%finalize()
  end subroutine final_xdmf

  subroutine write_xdmf_header(self)
    class(xdmf_contour_t), intent(inout) :: self
    call self%write_xml_header()
    call self%write_xml_grid()
  end subroutine

  subroutine xdmf_add_int_1d(self, name, data, units, description)
    class(xdmf_contour_t), intent(inout) :: self
    integer(ik), dimension(:), intent(in):: data
    character(*), intent(in) :: name
    character(*), intent(in) :: units
    character(*), intent(in) :: description

    character(:), allocatable :: dset_name
    dset_name = '/'//trim(name)
    call self%add_xml_variable_1d(name)
    call self%h5_file%add(dset_name, data)
    call self%h5_file%writeattr(dset_name, 'description', trim(description))
    call self%h5_file%writeattr(dset_name, 'units', trim(units))
  end subroutine xdmf_add_int_1d

  subroutine xdmf_add_int_2d(self, name, data, units, description)
    class(xdmf_contour_t), intent(inout) :: self
    integer(ik), dimension(:, :), intent(in):: data
    character(*), intent(in) :: name
    character(*), intent(in) :: units
    character(*), intent(in) :: description

    character(:), allocatable :: dset_name
    dset_name = '/'//trim(name)
    call self%add_xml_variable_2d(name, shape(data))
    call self%h5_file%add(dset_name, data)
    call self%h5_file%writeattr(dset_name, 'description', trim(description))
    call self%h5_file%writeattr(dset_name, 'units', trim(units))
  end subroutine xdmf_add_int_2d

  subroutine xdmf_add_int_3d(self, name, data, units, description)
    class(xdmf_contour_t), intent(inout) :: self
    integer(ik), dimension(:, :, :), intent(in):: data
    character(*), intent(in) :: name
    character(*), intent(in) :: units
    character(*), intent(in) :: description

    character(:), allocatable :: dset_name
    dset_name = '/'//trim(name)
    call self%add_xml_variable_3d(name, shape(data))
    call self%h5_file%add(dset_name, data)
    call self%h5_file%writeattr(dset_name, 'description', trim(description))
    call self%h5_file%writeattr(dset_name, 'units', trim(units))
  end subroutine xdmf_add_int_3d

  subroutine xdmf_add_real64_1d(self, name, data, units, description)
    class(xdmf_contour_t), intent(inout) :: self
    real(rk), dimension(:), intent(in) :: data
    character(*), intent(in) :: name
    character(*), intent(in) :: units
    character(*), intent(in) :: description

    character(:), allocatable :: dset_name
    dset_name = '/'//trim(name)
    call self%add_xml_variable_1d(name)
    call self%h5_file%add(dset_name, data)
    call self%h5_file%writeattr(dset_name, 'description', trim(description))
    call self%h5_file%writeattr(dset_name, 'units', trim(units))
  end subroutine xdmf_add_real64_1d

  subroutine xdmf_add_real64_2d(self, name, data, units, description)
    class(xdmf_contour_t), intent(inout) :: self
    real(rk), dimension(:, :), intent(in) :: data
    character(*), intent(in) :: name
    character(*), intent(in) :: units
    character(*), intent(in) :: description

    character(:), allocatable :: dset_name
    dset_name = '/'//trim(name)
    call self%add_xml_variable_2d(name, shape(data))
    call self%h5_file%add(dset_name, data)
    call self%h5_file%writeattr(dset_name, 'description', trim(description))
    call self%h5_file%writeattr(dset_name, 'units', trim(units))
  end subroutine xdmf_add_real64_2d

  subroutine xdmf_add_real64_3d(self, name, data, units, description)
    class(xdmf_contour_t), intent(inout) :: self
    real(rk), dimension(:, :, :), intent(in) :: data
    character(*), intent(in) :: name
    character(*), intent(in) :: units
    character(*), intent(in) :: description

    character(:), allocatable :: dset_name
    dset_name = '/'//trim(name)
    call self%add_xml_variable_3d(name, shape(data))
    call self%h5_file%add(dset_name, data)
    call self%h5_file%writeattr(dset_name, 'description', trim(description))
    call self%h5_file%writeattr(dset_name, 'units', trim(units))
  end subroutine xdmf_add_real64_3d

end module mod_xdmf

! <?xml version="1.0" ?>
! <Xdmf version="2.2">
!   <Domain>
!     <Grid GridType="Uniform" Name="grid">
!       <Time Value="2.3 nanosecond"/>
!       <Topology NumberOfElements="266 601 31" TopologyType="3DSMesh"/>
!       <Geometry GeometryType="X_Y_Z">
!         <DataItem Dimensions="266 601 31" Format="HDF" NumberType="Float" Precision="4">profile_126_2.300ns.h5:/x</DataItem>
!         <DataItem Dimensions="266 601 31" Format="HDF" NumberType="Float" Precision="4">profile_126_2.300ns.h5:/y</DataItem>
!         <DataItem Dimensions="266 601 31" Format="HDF" NumberType="Float" Precision="4">profile_126_2.300ns.h5:/z</DataItem>
!       </Geometry>
!       <Attribute AttributeType="Scalar" Center="Cell" Name="mass_density">
! <DataItem Dimensions="265 600 30" Format="HDF" NumberType="Float" Precision="4">profile_126_2.300ns.h5:/mass_density</DataItem>
!       </Attribute>
!       <Attribute AttributeType="Scalar" Center="Cell" Name="ion_temperature">
!         <DataItem Dimensions="265 600 30" Format="HDF" NumberType="Float" Precision="4">profile_126_2.300ns.h5:/ion_temperature</DataItem>
!       </Attribute>
!     </Grid>
!   </Domain>
! </Xdmf>
