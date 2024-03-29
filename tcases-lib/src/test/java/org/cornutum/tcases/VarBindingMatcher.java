//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2018, Cornutum Project
//                             www.cornutum.org
//                           All Rights Reserved
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases;

import org.cornutum.hamcrest.BaseCompositeMatcher;
import org.cornutum.tcases.util.ObjectUtils;

import org.hamcrest.Matchers;

/**
 * A composite matcher for {@link VarBinding} objects.
 */
public class VarBindingMatcher extends BaseCompositeMatcher<VarBinding>
  {
  /**
   * Creates a new VarBindingMatcher instance.
   */
  public VarBindingMatcher( VarBinding expected)
    {
    super( expected);

    expectThat( valueOf( "var", VarBinding::getVar).matches( Matchers::equalTo));
    expectThat( valueOf( "type", VarBinding::getType).matches( Matchers::equalTo));
    expectThat( valueOf( "value", this::getValue).matches( Matchers::equalTo));
    expectThat( valueOf( "source", this::getSource).matches( Matchers::equalTo));
    expectThat( valueOf( "applicable", (VarBinding vb) -> !vb.isValueNA()).matches( Matchers::equalTo));
    expectThat( valueOf( "valid", VarBinding::isValueValid).matches( Matchers::equalTo));
    expectThat( matches( AnnotatedMatcher::new));
    }

  private Object getValue( VarBinding binding)
    {
    return ObjectUtils.toExternalObject( binding.getValue());
    }

  private Object getSource( VarBinding binding)
    {
    return ObjectUtils.toExternalObject( binding.getSource());
    }
  }
