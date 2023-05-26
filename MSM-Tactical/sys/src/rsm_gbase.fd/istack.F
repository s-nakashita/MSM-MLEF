       module istack

       implicit none
       type stack 
          private 
          type (node), pointer ::top
       end type stack

       type, private  :: node
          integer (kind=4) value
          type (stack) :: rest_of_stack
       end type node

       contains

! initialize stack
       function istack_new () result(new_result)

       type (stack) :: new_result
       nullify (new_result%top)

       end function istack_new

! test if stack is empty
       function istack_empty (s) result(empty_result)

          type(stack), intent(in) :: s
          logical :: empty_result
   
           empty_result = .not. associated(s%top)
       end function istack_empty

! push a number into stack
       function istack_push (s,number) result(push_result)

           type(stack) :: push_result
           type(stack), intent (in) :: s
           integer, intent(in) :: number

            allocate(push_result%top)
            push_result%top%value=number
            push_result%top%rest_of_stack=s

       end function istack_push

! get the top number in stack
       function istack_peek (s) result(top_of_result)

           type (stack) :: s
           integer (kind=4) :: top_of_result
    
           if ( istack_empty(s) ) then
              top_of_result= -huge(top_of_result)
           else
              top_of_result=s%top%value
           endif

       end function istack_peek

! pop out a node, get rest of stack
       function istack_pop (s) result(rest_of_result)

           type(stack) :: rest_of_result
           type(stack) :: s

           if (istack_empty(s) ) then
             rest_of_result=s
           else
             rest_of_result=s%top%rest_of_stack
             deallocate(s%top)
           end if
   
       end function istack_pop

       function istack_deallocate (s) result(err)
                                                                                                         
           integer (kind=4) :: err
           type(stack) :: s
                                                                                                         
           if (.not.istack_empty(s) ) then
             err=istack_peek(s)
             s=istack_pop(s)
           else
             err=istack_peek(s)
           end if
                                                                                                         
       end function istack_deallocate


!skip select  and split not used in cnttime

       end module istack

