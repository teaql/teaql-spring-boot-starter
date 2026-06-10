package io.teaql.data;

import io.teaql.data.utils.ObjectUtil;

/**
 * 要求所有查询必须声明 comment 和 purpose 的策略。
 * 没有 purpose 的查询会被拒绝执行。
 *
 * 设计参考 teaql-rs 的 Triple-Intent 模式：
 * - comment: 描述这个查询加载什么数据
 * - purpose: 描述为什么需要这些数据（业务意图）
 *
 * 用法:
 *   ctx.setRequestPolicy(new PurposeRequestPolicy());
 *
 *   // 正确
 *   Q.tasks().comment("加载任务列表").purpose("展示看板").executeForList(ctx);
 *
 *   // 被拒绝: 缺少 purpose
 *   Q.tasks().executeForList(ctx);
 */
public class PurposeRequestPolicy implements RequestPolicy {

    @Override
    public void enforceSelect(UserContext ctx, SearchRequest<?> query) {
        String typeName = query.getTypeName();
        String comment = query.comment();
        String purpose = query.purpose();

        boolean missingComment = ObjectUtil.isEmpty(comment);
        boolean missingPurpose = ObjectUtil.isEmpty(purpose);

        if (!missingComment && !missingPurpose) {
            return;
        }

        StringBuilder msg = new StringBuilder();
        msg.append("[PURPOSE REQUIRED] Query on ").append(typeName).append(" rejected.\n");
        msg.append("Missing: ");
        if (missingComment) msg.append(".comment() ");
        if (missingPurpose) msg.append(".purpose() ");
        msg.append("\n\n");
        msg.append("FIX: Every query must declare both .comment() and .purpose() before execution.\n");
        msg.append("Correct pattern:\n");
        msg.append("  Q.").append(uncapFirst(typeName)).append("s()\n");
        msg.append("      .filterByXxx(...)\n");
        msg.append("      .comment(\"Describe what this query loads\")\n");
        msg.append("      .purpose(\"Describe why this data is needed\")\n");
        msg.append("      .executeForList(ctx);\n");

        throw new RepositoryException(msg.toString());
    }

    @Override
    public void enforceInsert(UserContext ctx, Entity entity) {
        enforceAuditComment(ctx, entity, "insert");
    }

    @Override
    public void enforceUpdate(UserContext ctx, Entity entity) {
        enforceAuditComment(ctx, entity, "update");
    }

    @Override
    public void enforceDelete(UserContext ctx, Entity entity) {
        enforceAuditComment(ctx, entity, "delete");
    }

    @Override
    public void enforceRecover(UserContext ctx, Entity entity) {
        enforceAuditComment(ctx, entity, "recover");
    }

    private void enforceAuditComment(UserContext ctx, Entity entity, String operation) {
        String comment = entity.getComment();
        if (ObjectUtil.isNotEmpty(comment)) {
            return;
        }

        String typeName = entity.typeName();
        StringBuilder msg = new StringBuilder();
        msg.append("[AUDIT REQUIRED] ").append(operation).append(" on ").append(typeName);
        msg.append("(id=").append(entity.getId()).append(") rejected.\n");
        msg.append("Missing: .auditAs()\n\n");
        msg.append("FIX: Every entity mutation must call .auditAs() before save.\n");
        msg.append("Correct pattern:\n");
        msg.append("  entity.updateXxx(newValue)\n");
        msg.append("      .auditAs(\"Describe the business action\")\n");
        msg.append("      .save(ctx);\n");

        throw new RepositoryException(msg.toString());
    }

    private static String uncapFirst(String s) {
        if (s == null || s.isEmpty()) return s;
        return Character.toLowerCase(s.charAt(0)) + s.substring(1);
    }
}
