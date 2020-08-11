package org.cornutum.moco;


import org.junit.Test;

import com.github.dreamhead.moco.junit.MocoJunitRunner;
import static com.github.dreamhead.moco.Moco.*;
import org.junit.ClassRule;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class OpenAPIRequestTestCasesTest extends MyBaseClass {

    @ClassRule
    public static MocoJunitRunner runner = MocoJunitRunner.jsonRestRunner( 12306, file( "/Users/kerrykimbrough/repos/tcases/tcases-cli/target/test-classes/org/cornutum/tcases/openapi/myRestServerConfig.json"));

    @Test
    public void headPost_UserAttributesDefined_Is_Yes() {
        given()
            .queryParam( "post?[post-references]", "0,1")
            .queryParam( "user attributes[user-type]", "Typical User")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeValue_Is_VIP() {
        given()
            .queryParam( "post?[post-references]", "1,2")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsValue_Is_2() {
        given()
            .queryParam( "post?[post-references]", "2,1")
            .queryParam( "user attributes[user-type]", "Typical User")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void headPost_UserAttributesDefined_Is_No() {
        given()
            .queryParam( "post?[post-references]", "0,2")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // user-attributes.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_UserAttributesType_Is_Null() {
        given()
            .queryParam( "post?[post-references]", "0,2")
            .queryParam( "user attributes", "")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // user-attributes.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_UserAttributesType_Is_NotObject() {
        given()
            .queryParam( "post?[post-references]", "0,2")
            .queryParam( "user attributes", "-163.3")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // user-attributes.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeDefined_Is_No() {
        given()
            .queryParam( "post?[post-references]", "0,2")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // user-attributes.Value.Properties.user-type.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeType_Is_Null() {
        given()
            .queryParam( "post?[post-references]", "0,2")
            .queryParam( "user attributes[user-type]", "")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // user-attributes.Value.Properties.user-type.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_UserAttributesValuePropertiesUserTypeValue_Is_Other() {
        given()
            .queryParam( "post?[post-references]", "0,1")
            .queryParam( "user attributes[user-type]", "*76s/1*r")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // user-attributes.Value.Properties.user-type.Value=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_UserAttributesValuePropertiesAdditional_Is_Yes() {
        given()
            .queryParam( "post?[post-references]", "0,2")
            .queryParam( "user attributes[user-type]", "VIP!")
            .queryParam( "user attributes[wfgawhzebqxw]", "-993.3")
            .queryParam( "user attributes[bt]", "-646")
            .queryParam( "user attributes[d]", "429")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // user-attributes.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostDefined_Is_No() {
        given()
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // post.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostType_Is_Null() {
        given()
            .queryParam( "post?", "")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // post.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostType_Is_NotObject() {
        given()
            .queryParam( "post?", "-689.9")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // post.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesDefined_Is_No() {
        given()
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // post.Value.Properties.post-references.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesType_Is_Null() {
        given()
            .queryParam( "post?[post-references]", "")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // post.Value.Properties.post-references.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesType_Is_NotArray() {
        given()
            .queryParam( "post?[post-references]", "true")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // post.Value.Properties.post-references.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsSize_Is_1() {
        given()
            .queryParam( "post?[post-references]", "0")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // post.Value.Properties.post-references.Items.Size=1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsSize_Is_3() {
        given()
            .queryParam( "post?[post-references]", "0,1,2")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // post.Value.Properties.post-references.Items.Size=3
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsType_Is_Null() {
        given()
            .queryParam( "post?[post-references]", ",1")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // post.Value.Properties.post-references.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsType_Is_NotInteger() {
        given()
            .queryParam( "post?[post-references]", ",,NU,2")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // post.Value.Properties.post-references.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsContainsValue_Is_Other() {
        given()
            .queryParam( "post?[post-references]", "518413185,0")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // post.Value.Properties.post-references.Items.Contains.Value.Is=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesPostReferencesItemsUnique_Is_No() {
        given()
            .queryParam( "post?[post-references]", "0,0")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // post.Value.Properties.post-references.Items.Unique=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void headPost_PostValuePropertiesAdditional_Is_Yes() {
        given()
            .queryParam( "post?[post-references]", "0,1")
            .queryParam( "post?[pfuxkykifiozux]", "g\"YtKR,&")
            .queryParam( "user attributes[user-type]", "VIP!")
        .when()
            .request( "HEAD", "http://localhost:12306/post")
        .then()
            // post.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void patchPost_PostMarksDefined_Is_Yes() {
        given()
            .queryParam( "Post Marks", "{X}")
        .when()
            .request( "PATCH", "http://localhost:12306/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsSize_Is_3() {
        given()
            .queryParam( "Post Marks", "<Y> {X} #Z")
        .when()
            .request( "PATCH", "http://localhost:12306/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsContainsValue_Is_Z() {
        given()
            .queryParam( "Post Marks", "#Z")
        .when()
            .request( "PATCH", "http://localhost:12306/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsUnique_Is_No() {
        given()
            .queryParam( "Post Marks", "{X} {X} {X}")
        .when()
            .request( "PATCH", "http://localhost:12306/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void patchPost_PostMarksDefined_Is_No() {
        given()
        .when()
            .request( "PATCH", "http://localhost:12306/post")
        .then()
            // Post-Marks.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void patchPost_PostMarksType_Is_Null() {
        given()
            .queryParam( "Post Marks", "")
        .when()
            .request( "PATCH", "http://localhost:12306/post")
        .then()
            // Post-Marks.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void patchPost_PostMarksType_Is_NotArray() {
        given()
            .queryParam( "Post Marks", "+")
        .when()
            .request( "PATCH", "http://localhost:12306/post")
        .then()
            // Post-Marks.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsSize_Is_0() {
        given()
            .queryParam( "Post Marks", "")
        .when()
            .request( "PATCH", "http://localhost:12306/post")
        .then()
            // Post-Marks.Items.Size=0
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsSize_Is_4() {
        given()
            .queryParam( "Post Marks", "<Y> <Y> <Y> {X}")
        .when()
            .request( "PATCH", "http://localhost:12306/post")
        .then()
            // Post-Marks.Items.Size=4
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsContainsType_Is_Null() {
        given()
            .queryParam( "Post Marks", "")
        .when()
            .request( "PATCH", "http://localhost:12306/post")
        .then()
            // Post-Marks.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void patchPost_PostMarksItemsContainsValue_Is_Other() {
        given()
            .queryParam( "Post Marks", "#/lL\"%`\\M,7/RPK2]eZ/b\"LtW=MB6,(")
        .when()
            .request( "PATCH", "http://localhost:12306/post")
        .then()
            // Post-Marks.Items.Contains.Value=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_PostIdDefined_Is_Yes() {
        given()
            .queryParam( "postId", "0")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "true")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void putPost_PostIdValue_Is_Gt_0() {
        given()
            .queryParam( "postId", "884128300094585099.3")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "(?)")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerValue_Is_MeYou() {
        given()
            .queryParam( "postId", "0")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "true")
            .formParam( "reviewer", "Me+You")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void putPost_PostIdDefined_Is_No() {
        given()
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // postId.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_PostIdType_Is_Null() {
        given()
            .queryParam( "postId", "")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // postId.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_PostIdType_Is_NotNumber() {
        given()
            .queryParam( "postId", "%")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // postId.Type=Not number
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_PostIdValue_Is_M1() {
        given()
            .queryParam( "postId", "-1")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // postId.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyDefined_Is_No() {
        given()
            .queryParam( "postId", "579110988210992054.5")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // Body.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyMediaType_Is_Other() {
        given()
            .queryParam( "postId", "100055597218570470.8")
            .contentType( "application/xml")
            .request().body( ">jg-FQI")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // Body.Media-Type=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedType_Is_Null() {
        given()
            .queryParam( "postId", "545768800747318227.7")
            .contentType( "application/x-www-form-urlencoded")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // Body.application-x-www-form-urlencoded.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedType_Is_NotObject() {
        given()
            .queryParam( "postId", "787781271506673512.7")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "string", "")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // Body.application-x-www-form-urlencoded.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedDefined_Is_No() {
        given()
            .queryParam( "postId", "887390451195556957.7")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.approved.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedType_Is_Null() {
        given()
            .queryParam( "postId", "218911377319422868.8")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.approved.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedType_Is_NotBoolean() {
        given()
            .queryParam( "postId", "847512139010470218.2")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "eytx,7ea{")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.approved.Type=Not boolean
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerDefined_Is_No() {
        given()
            .queryParam( "postId", "711003745143914146.9")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerType_Is_Null() {
        given()
            .queryParam( "postId", "167771822150204639.4")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerValue_Is_Other() {
        given()
            .queryParam( "postId", "886308504886987482.6")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "i&")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Value=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesAdditional_Is_Yes() {
        given()
            .queryParam( "postId", "920816794015899048.8")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
            .formParam( "dxobuuyffc", "true")
            .formParam( "wssfr", "true")
        .when()
            .request( "PUT", "http://localhost:12306/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    private Matcher<Integer> isSuccess() {
        return allOf( greaterThanOrEqualTo(200), lessThan(300));
    }

    private Matcher<Integer> isBadRequest() {
        return allOf( greaterThanOrEqualTo(400), lessThan(500));
    }
}
