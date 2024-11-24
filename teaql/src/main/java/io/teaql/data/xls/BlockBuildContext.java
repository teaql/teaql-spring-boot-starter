package io.teaql.data.xls;

/** the current block, we will generate other blocks based on this block. */
public class BlockBuildContext {
  private String page;

  private int startX;
  private int x;
  private int y;

  public BlockBuildContext(String pPage, int pX, int pY) {
    page = pPage;
    startX = pX;
    x = pX;
    y = pY;
  }

  public BlockBuildContext(String pPage) {
    page = pPage;
  }

  public BlockBuildContext next() {
    BlockBuildContext blockBuildContext = new BlockBuildContext(this.page, this.x + 1, this.y);
    blockBuildContext.startX = this.startX;
    return blockBuildContext;
  }

  public BlockBuildContext newLine() {
    BlockBuildContext blockBuildContext = new BlockBuildContext(this.page, 0, this.y + 1);
    blockBuildContext.startX = this.startX;
    return blockBuildContext;
  }

  public BlockBuildContext nextLine() {
    return new BlockBuildContext(this.page, startX, this.y + 1);
  }

  public String getPage() {
    return page;
  }

  public int getX() {
    return x;
  }

  public int getY() {
    return y;
  }

  public Block toBlock(Object value) {
    return new Block(this, value);
  }
}
