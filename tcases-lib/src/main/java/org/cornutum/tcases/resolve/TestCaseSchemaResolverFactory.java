//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2022, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.resolve;

import org.cornutum.tcases.*;

/**
 * Creates a {@link TestCaseSchemaResolver} for a {@link FunctionInputDef function input definition}
 */
public class TestCaseSchemaResolverFactory implements TestCaseResolverFactory
  {
  /**
   * Returns a new {@link TestCaseSchemaResolverFactory} instance.
   */
  public TestCaseSchemaResolverFactory( ResolverContext context)
    {
    context_ = context;
    }

  /**
   * Returns a {@link TestCaseResolver} for the given {@link FunctionInputDef function input definition}
   */
  @Override
  public TestCaseResolver resolverFor( FunctionInputDef inputDef)
    {
    return new TestCaseSchemaResolver( getResolverContext(), inputDef);
    }

  /**
   * Returns the {@link ResolverContext} for this factory.
   */
  @Override
  public ResolverContext getResolverContext()
    {
    return context_;
    }

  private final ResolverContext context_;
  }
