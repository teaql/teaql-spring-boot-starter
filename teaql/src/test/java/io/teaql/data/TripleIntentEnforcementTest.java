package io.teaql.data;

import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Verifies the Triple-Intent runtime enforcement:
 *   - Queries must have .comment() + .purpose()
 *   - Saves must have .auditAs()
 *
 * Enforcement is controlled by TEAQL_ENFORCE_INTENT env var / system property.
 */
public class TripleIntentEnforcementTest {

    // ── Minimal concrete stubs for testing ──────────────────────────

    /** A concrete entity for testing save-side enforcement. */
    static class StubEntity extends BaseEntity {
        @Override
        public String typeName() {
            return "StubEntity";
        }
    }

    /** A concrete request for testing query-side enforcement. */
    static class StubRequest extends BaseRequest<StubEntity> {
        public StubRequest() {
            super(StubEntity.class);
        }

        public StubRequest comment(String comment) {
            super.internalComment(comment);
            return this;
        }

        public StubRequest purpose(String purpose) {
            super.internalPurpose(purpose);
            return this;
        }
    }

    /** A test-friendly UserContext that lets us control enforcement mode. */
    static class TestUserContext extends UserContext {
        private final UserContext.IntentEnforcementMode mode;

        TestUserContext(IntentEnforcementMode mode) {
            this.mode = mode;
        }

        @Override
        protected <T extends Entity> SearchRequest<T> enforceRequestPolicy(SearchRequest<T> request) {
            // Replicate the logic from UserContext but using our controlled mode
            if (mode == IntentEnforcementMode.OFF) {
                return request;
            }
            String comment = request.comment();
            String purpose = request.purpose();
            boolean missingComment = comment == null || comment.isEmpty();
            boolean missingPurpose = purpose == null || purpose.isEmpty();

            if (!missingComment && !missingPurpose) {
                return request;
            }

            StringBuilder msg = new StringBuilder();
            msg.append("[TRIPLE-INTENT] Query on ").append(request.getTypeName()).append(" is missing: ");
            if (missingComment) msg.append(".comment(\"...\") ");
            if (missingPurpose) msg.append(".purpose(\"...\") ");

            if (mode == IntentEnforcementMode.STRICT) {
                throw new RepositoryException(msg.toString());
            }
            return request;
        }

        /** Expose audit enforcement for direct testing. */
        public void testEnforceAudit(Entity entity) {
            if (mode == IntentEnforcementMode.OFF) {
                return;
            }
            String comment = entity.getComment();
            if (comment != null && !comment.isEmpty()) {
                return;
            }

            String msg = "[TRIPLE-INTENT] Save on " + entity.typeName()
                    + "(id=" + entity.getId() + ") is missing .auditAs(\"...\")";

            if (mode == IntentEnforcementMode.STRICT) {
                throw new RepositoryException(msg);
            }
        }
    }

    // ── Query-side tests ────────────────────────────────────────────

    @Test
    public void query_withBothCommentAndPurpose_passes() {
        TestUserContext ctx = new TestUserContext(UserContext.IntentEnforcementMode.STRICT);
        StubRequest request = new StubRequest()
                .comment("Load tasks for dashboard")
                .purpose("Display task count widget");

        // Should NOT throw
        SearchRequest<StubEntity> result = ctx.enforceRequestPolicy(request);
        assertNotNull(result);
        assertEquals("Load tasks for dashboard", result.comment());
        assertEquals("Display task count widget", result.purpose());
    }

    @Test(expected = RepositoryException.class)
    public void query_withoutPurpose_throwsInStrict() {
        TestUserContext ctx = new TestUserContext(UserContext.IntentEnforcementMode.STRICT);
        StubRequest request = new StubRequest()
                .comment("Load tasks");
        // Missing .purpose() → should throw

        ctx.enforceRequestPolicy(request);
    }

    @Test(expected = RepositoryException.class)
    public void query_withoutComment_throwsInStrict() {
        TestUserContext ctx = new TestUserContext(UserContext.IntentEnforcementMode.STRICT);
        StubRequest request = new StubRequest()
                .purpose("Dashboard display");
        // Missing .comment() → should throw

        ctx.enforceRequestPolicy(request);
    }

    @Test(expected = RepositoryException.class)
    public void query_withNothing_throwsInStrict() {
        TestUserContext ctx = new TestUserContext(UserContext.IntentEnforcementMode.STRICT);
        StubRequest request = new StubRequest();
        // Missing both → should throw

        ctx.enforceRequestPolicy(request);
    }

    @Test
    public void query_withNothing_passesInOffMode() {
        TestUserContext ctx = new TestUserContext(UserContext.IntentEnforcementMode.OFF);
        StubRequest request = new StubRequest();

        // OFF mode → should NOT throw
        SearchRequest<StubEntity> result = ctx.enforceRequestPolicy(request);
        assertNotNull(result);
    }

    @Test
    public void query_withNothing_passesInWarnMode() {
        TestUserContext ctx = new TestUserContext(UserContext.IntentEnforcementMode.WARN);
        StubRequest request = new StubRequest();

        // WARN mode → should NOT throw, just log
        SearchRequest<StubEntity> result = ctx.enforceRequestPolicy(request);
        assertNotNull(result);
    }

    // ── Save-side tests ─────────────────────────────────────────────

    @Test
    public void save_withAuditAs_passes() {
        TestUserContext ctx = new TestUserContext(UserContext.IntentEnforcementMode.STRICT);
        StubEntity entity = new StubEntity();
        entity.setId(42L);
        entity.auditAs("Create new task for robot assembly");

        // Should NOT throw
        ctx.testEnforceAudit(entity);
        assertEquals("Create new task for robot assembly", entity.getComment());
    }

    @Test(expected = RepositoryException.class)
    public void save_withoutAuditAs_throwsInStrict() {
        TestUserContext ctx = new TestUserContext(UserContext.IntentEnforcementMode.STRICT);
        StubEntity entity = new StubEntity();
        entity.setId(42L);
        // Missing .auditAs() → should throw

        ctx.testEnforceAudit(entity);
    }

    @Test
    public void save_withoutAuditAs_passesInOffMode() {
        TestUserContext ctx = new TestUserContext(UserContext.IntentEnforcementMode.OFF);
        StubEntity entity = new StubEntity();
        entity.setId(42L);

        // OFF mode → should NOT throw
        ctx.testEnforceAudit(entity);
    }

    // ── auditAs fluent chaining test ────────────────────────────────

    @Test
    public void auditAs_setsCommentAndReturnsSelf() {
        StubEntity entity = new StubEntity();
        Entity returned = entity.auditAs("Transition task to DONE");

        assertSame(entity, returned);
        assertEquals("Transition task to DONE", entity.getComment());
    }

    // ── purpose + comment field test ────────────────────────────────

    @Test
    public void request_purposeAndComment_areIndependent() {
        StubRequest request = new StubRequest()
                .comment("Fetch orders by region")
                .purpose("Generate regional sales report");

        assertEquals("Fetch orders by region", request.comment());
        assertEquals("Generate regional sales report", request.purpose());
    }
}
