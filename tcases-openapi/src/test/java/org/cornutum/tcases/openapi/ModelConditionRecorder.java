//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi;

import org.apache.commons.collections4.ListValuedMap;
import org.apache.commons.collections4.MultiMapUtils;

import java.util.List;

/**
 * A {@link ModelConditionNotifier} keeps a record of all conditions notified.
 */
public class ModelConditionRecorder implements ModelConditionNotifier
  {
  @Override
public void warn( String[] location, String reason)
    {
    conditions_.put( "warn", messageFor( location, reason, null));
    }

  @Override
public void error( String[] location, String reason, String resolution)
    {
    conditions_.put( "error", messageFor( location, reason, resolution));
    }

  public List<String> getWarnings()
    {
    return conditions_.get( "warn");
    }

  public List<String> getErrors()
    {
    return conditions_.get( "error");
    }
      
  private ListValuedMap<String,String> conditions_ = MultiMapUtils.newListValuedHashMap();
  }
