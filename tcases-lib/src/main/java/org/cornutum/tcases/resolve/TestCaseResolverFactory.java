//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.*;
import org.cornutum.tcases.util.Notifier;

/**
 * Creates a {@link TestCaseResolver} for a {@link FunctionInputDef function input definition}
 */
public interface TestCaseResolverFactory
  {
  /**
   * Returns a {@link TestCaseResolver} for the given {@link FunctionInputDef function input definition}
   */
  public TestCaseResolver resolverFor( FunctionInputDef inputDef);

  /**
   * Returns the {@link ResolverContext} for this factory.
   */
  public ResolverContext getResolverContext();

  /**
   * Returns a {@link TestCaseResolver} uses only the basic input model without schemas.
   */
  public static TestCaseResolverFactory DEFAULT =
    new TestCaseResolverFactory()
      {
      @Override
      public TestCaseResolver resolverFor( FunctionInputDef inputDef)
        {
        return
          new TestCaseResolver( getResolverContext(), inputDef)
            {
            @Override
            protected VarBinding resolveBinding( VarDef varDef, VarValueDef valueDef)
              {
              return VarBinding.create( varDef, valueDef);
              }
          };
        }
      
      /**
       * Returns the {@link ResolverContext} for this factory.
       */
      @Override
      public ResolverContext getResolverContext()
        {
        return ResolverContext.builder().notifier( Notifier.ignore()).build();
        }
      };
  }
