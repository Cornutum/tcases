//////////////////////////////////////////////////////////////////////////////
// 
//                    Copyright 2020, Cornutum Project
//                             www.cornutum.org
//
//////////////////////////////////////////////////////////////////////////////

package org.cornutum.tcases.openapi.resolver;

/**
 * A composite matcher for {@link MessageData} objects.
 */
public class MessageDataMatcher extends AbstractMessageDataMatcher<MessageData>
  {
  /**
   * Creates a new MessageDataMatcher instance.
   */
  public MessageDataMatcher( MessageData expected)
    {
    super( expected);
    }
  }
