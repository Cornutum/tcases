package org.cornutum.tcases.annotation.util;

import org.apache.commons.lang3.builder.RecursiveToStringStyle;

/**
 * helper for reflection-based toString
 * TODO: Replace with proper toString once Model became stable
 */
public class CustomToStringStyle extends RecursiveToStringStyle {
    public static CustomToStringStyle INSTANCE = new CustomToStringStyle();
    // for Json-like toString
    public CustomToStringStyle() {
        super();
        super.setUseClassName(false);
        super.setUseIdentityHashCode(false);
        super.setContentStart("{");
        super.setContentEnd("}");
        super.setArrayStart("[");
        super.setArrayEnd("]");
        super.setFieldSeparator(", ");
        super.setFieldNameValueSeparator(":");
        super.setNullText("null");
        super.setSummaryObjectStartText("\"<");
        super.setSummaryObjectEndText(">\"");
        super.setSizeStartText("\"<size=");
        super.setSizeEndText(">\"");
    }
}