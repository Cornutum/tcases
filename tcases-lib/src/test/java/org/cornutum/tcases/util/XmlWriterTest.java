package org.cornutum.tcases.util;

import org.junit.Test;

import java.io.StringWriter;

import org.apache.commons.io.IOUtils;
import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

public class XmlWriterTest
  {

  @Test
  public void writeAttributeWithValue()
    {
    assertThat( writeAttributeWithValue(""), is(" foo=\"\""));
    assertThat( writeAttributeWithValue("bar"), is(" foo=\"bar\""));
    assertThat( writeAttributeWithValue("\t"), is(" foo=\"&#9;\""));
    assertThat( writeAttributeWithValue("<"), is(" foo=\"&lt;\""));
    assertThat( writeAttributeWithValue("&"), is(" foo=\"&amp;\""));
    assertThat( writeAttributeWithValue("\""), is(" foo=\"&quot;\""));
    assertThat( writeAttributeWithValue("\r"), is(" foo=\"&#13;\""));
    assertThat( writeAttributeWithValue("あ"), is(" foo=\"あ\""));
    }

  private static String writeAttributeWithValue(String input)
    {
    XmlWriter xmlWriter = null;
    try
      {
      StringWriter writer = new StringWriter();
      xmlWriter = new XmlWriter(writer);
      xmlWriter.writeAttribute("foo", input);
      return writer.toString();
      }
    finally
      {
      IOUtils.closeQuietly(xmlWriter);
      }
    }
  }
