//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2019, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.cornutum.tcases.*;
import static org.cornutum.tcases.util.CollectionUtils.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Represents a binding for a variable that belongs to the input model for a composed schema member.
 */
public class MemberVarBinding extends VarBinding
  {
  /**
   * Creates a new MemberVarBinding instance.
   */
  public MemberVarBinding( IVarDef varDef, VarValueDef valueDef)
    {
    super( varDef.getPathName(), varDef.getType(), valueDef.getName());
    setValueValid( valueDef.isValid());
    isNA_ = valueDef.isNA();
    }

  /**
   * Returns the set of failure bindings for the given member input variables.
   */
  public static List<MemberVarBinding> getFailureBindings( List<IVarDef> memberVars)
    {
    return
      toStream( new VarDefIterator( memberVars.iterator()))
      .flatMap( varDef -> toStream( varDef.getValues()).map( valueDef -> new MemberVarBinding( varDef, valueDef)))
      .filter( binding -> !binding.isValueValid())
      .collect(
        ArrayList::new,
        (bindings,failure) -> bindings.add( 0, failure),
        ArrayList::addAll);
    }

  /**
   * Returns the designated failure binding for each member of a oneOf/anyOf schema.
   * The i'th element of returned list is the designated failure for the i'th member, which
   * is modelled by the input variables listed in the i'th element of the <CODE>oneOfVars</CODE> list.
   */
  public static List<MemberVarBinding> getMemberFailures( List<List<IVarDef>> oneOfMembers)
    {
    return
      oneOfMembers.stream()
      .map( memberVars -> getFailureBindings( memberVars))
      .collect(
        ArrayList::new,
        (failures,bindings) ->
          failures.add(
            // Any failure bindings for this member?
            bindings.isEmpty()?
            // No
            null :

            // Yes, designate a binding not already used by a different member...
            bindings.stream()
            .filter( binding -> !failures.contains( binding))
            .findFirst()
            // ... or if none found, designate first binding
            .orElse( bindings.get(0))),
        ArrayList::addAll);
    }

  /**
   * Returns true if this binding indicates a "not applicable" condition for this variable.
   */
  public boolean isValueNA()
    {
    return isNA_;
    }

  public boolean equals( Object object)
    {
    MemberVarBinding other =
      object != null && object.getClass().equals( getClass())
      ? (MemberVarBinding) object
      : null;

    return
      other != null
      && Objects.equals( other.getVar(), getVar())
      && Objects.equals( other.getValue(), getValue())
      && other.isValueNA() == isValueNA();
    }

  public int hashCode()
    {
    return
      getClass().hashCode()
      ^ Objects.hashCode( getVar())
      ^ Objects.hashCode( getValue())
      ^ Boolean.valueOf( isValueNA()).hashCode();
    }
  
  private final boolean isNA_;
  }
